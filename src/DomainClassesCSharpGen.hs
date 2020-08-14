module DomainClassesCSharpGen
    ( transform
    ) where

import Parsing
import Language.CSharp.Syntax
import CSharpGen
import Data.Maybe (catMaybes)

transform :: DslVal -> Either String CompilationUnit
transform (Namespace usings ns cs) = Right $ CompilationUnit (mkUsings usings) [mkNamespace ns (map transformClass cs)]

transformClass :: Class -> Declaration
transformClass (Class c ps) = transformClassC c ps

transformClassC :: String -> [Property] -> Declaration
transformClassC c ps = mkPublicClass c (if hasOtherValidationsPs then (generateStaticCreateMethod:ctorAndProperties) else ctorAndProperties)
    where 
        hasOtherValidationsP (Property _ _ vals) = not $ null $ filter (/=NotNull) vals
        hasNotNullValidationsP (Property _ _ vals) = not $ null $ filter (==NotNull) vals
        hasNotNullValidationsPs = or $ map hasNotNullValidationsP ps
        hasOtherValidationsPs = or $ map hasOtherValidationsP ps

        generateStaticCreateMethod = mkPublicStaticCreateMethod c ps

        ctorAndProperties = generateCtor : generateProperties
        applyNotNullInCtor = hasNotNullValidationsPs && (not hasOtherValidationsPs)
        generateCtor = mkCtor [if(applyNotNullInCtor || not hasOtherValidationsPs) then Public else Private] applyNotNullInCtor c ps
        generateProperties = createProperties ps
        
createProperties :: [Property] -> [MemberDeclaration]
createProperties ps = map mkAutoProperty ps
    where 
        mkAutoProperty (Property n t _) = mkPropertyAutoPublicGet t n


mkCtor :: [Modifier] -> Bool -> String -> [Property] -> MemberDeclaration
mkCtor m applyNotNull c ps = 
    ConstructorMemberDeclaration 
        [] 
        m 
        (Identifier c) 
        (FormalParams (mkParams) (Nothing)) 
        Nothing 
        (mkPrivateCtorBody)
    where 
        mkParams = map mkFormalParamP ps
        mkPrivateCtorBody = ConstructorStatementBody (mkValidationsCtor ++ (map mkPrivateCtorBodyP ps))

        mkFormalParamP (Property n t _) = mkFormalParam t (camelCase n)
        mkPrivateCtorBodyP (Property n t vals) = ExpressionStatement $ mkAssignThisDot n (camelCase n)

        mkValidationsCtor = if(applyNotNull) then catMaybes $ map mkValidationCtor ps else []
        mkValidationCtor (Property n t vals) = 
            if hasNotNullValidations 
            then Just $ ifNullThen n (Throw (Just $ mkNew "System.ArgumentNullException" [mkLiteralStringArgument $ camelCase n, mkLiteralStringArgument $ "Field "++ camelCase n ++" with type "++ t ++ " can not be null"])) 
            else Nothing
            where 
                hasNotNullValidations = not $ null $ filter (==NotNull) vals
                ifNullThen n = ifThenBinaryOp BinaryEquals (mkSimpleName $ camelCase n) (Literal NullLit)
                

mkPublicStaticCreateMethod :: [Char] -> [Property] -> MemberDeclaration
mkPublicStaticCreateMethod c ps = 
    MethodMemberDeclaration 
        [] 
        [Public, Static] 
        (Just $ mkTypeNamed $ "Choice<"++ c ++ ", " ++ cError ++">") 
        (mkName "Create") 
        [] 
        (FormalParams (map mkFormalParamP ps) Nothing) 
        [] 
        (MethodStatementBody (mkAllValidations ++  [mkReturnCtorOk]))
    where
        cError = c ++ "Error"
        choiceT = "Choice<"++ c ++ ", " ++ cError ++ ">";
        invoke m = Invocation (mkSimpleName $ choiceT ++ "." ++ m)
        choice1Of2 = invoke "Choice1Of2"
        choice2Of2 = invoke "Choice2Of2"
        
        
        mkReturnCtorOk = mkReturn (choice1Of2 [mkArgument callCtor])
        callCtor = mkNew c (map mkSimpleNameArgumentP ps)
        mkAllValidations = concat $ map mkValidations ps

        mkFormalParamP (Property n t _) = mkFormalParam t (camelCase n)
        mkSimpleNameArgumentP (Property n _ _) = mkSimpleNameArgument (camelCase n)
        mkValidations (Property n t vals) = map (mkValidation t (camelCase n)) vals

        ifNullThen n = ifThenBinaryOp BinaryEquals (mkSimpleName n) (Literal NullLit)

        ifLengthBinaryOpThen op n l = ifThenBinaryOp op (mkSimpleName (n++".Length")) (Literal $ IntLit l)
        ifLengthGreaterThanThen = ifLengthBinaryOpThen BinaryGreaterThan
        ifLengthLessThanThen = ifLengthBinaryOpThen  BinaryLessThan

        ifEqualsThen n1 n2 = ifThen (Invocation (MemberAccess (PrimaryMemberAccess (mkSimpleName n1) (Identifier "Equals") [])) [Argument Nothing (mkSimpleName n2)])
        ifNotEqualsThen n1 n2 = ifThen (UnaryNot (Invocation (MemberAccess (PrimaryMemberAccess (mkSimpleName n1) (Identifier "Equals") [])) [Argument Nothing (mkSimpleName n2)]))

        returnError e = (mkReturn (choice2Of2 [mkSimpleNameArgument $ cError ++ "." ++ e]))
        
        mkValidation t n (NotNull) = ifNullThen n (Throw (Just $ mkNew "System.ArgumentNullException" [mkLiteralStringArgument n, mkLiteralStringArgument $ "Field "++ camelCase n ++" with type "++ t ++ " can not be null"])) 
        mkValidation _ n (MaxLength l) = ifLengthGreaterThanThen n l (returnError $ "MaxLength" ++ n ++ "Error")
        mkValidation _ n (MinLength l) = ifLengthLessThanThen n l (returnError $ "MinLength" ++ n ++ "Error")
        mkValidation _ n (Required) = ifNullThen n (returnError $ (capitalize n) ++ "FieldIsRequired")
        mkValidation _ n (Equals n2) = ifNotEqualsThen (camelCase n2) n (returnError $ n2 ++ "And"++ (capitalize n) ++ "NotEquals")
        mkValidation _ n (NotEquals n2) = ifEqualsThen (camelCase n2) n (returnError $ n2 ++ "And"++ (capitalize n) ++ "Equals")
