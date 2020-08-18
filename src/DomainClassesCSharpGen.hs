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

hasValidationsP :: (a -> Bool) -> [a] -> Bool
hasValidationsP predicate = not . null . filter predicate

hasNotNullValidationsP :: [PropertyValidation] -> Bool
hasNotNullValidationsP = hasValidationsP (==NotNull)
        
        
transformClassC :: String -> [Property] -> Declaration
transformClassC c ps = mkPublicClass c (if hasOtherValidationsPs then (generateStaticCreateMethod:ctorAndProperties) else ctorAndProperties)
    where 
        getVals (Property _ _ vals) = vals
        
        hasValidationsPs predicate = or $ map (predicate . getVals) ps

        hasOtherValidationsP = hasValidationsP (/=NotNull)
        hasOtherValidationsPs = hasValidationsPs hasOtherValidationsP

        hasNotNullValidationsPs = hasValidationsPs hasNotNullValidationsP
        
        generateStaticCreateMethod = mkPublicStaticCreateMethod c ps

        ctorAndProperties = generateCtor : generateProperties
        applyNotNullInCtor = hasNotNullValidationsPs && (not hasOtherValidationsPs)
        generateCtor = mkCtor [if(applyNotNullInCtor || not hasOtherValidationsPs) then Public else Private] applyNotNullInCtor c ps
        generateProperties = createProperties ps
        
createProperties :: [Property] -> [MemberDeclaration]
createProperties ps = map mkAutoProperty ps
    where 
        mkAutoProperty (Property n t _) = mkPropertyAutoPublicGet t n

mkFormalParamP :: Property -> FormalParam
mkFormalParamP (Property n t _) = mkFormalParam t (camelCase n)
        
mkFormalParamPs :: [Property] -> [FormalParam]
mkFormalParamPs ps = map mkFormalParamP ps
        
mkCtor :: [Modifier] -> Bool -> String -> [Property] -> MemberDeclaration
mkCtor modifiers applyNotNull c ps = mkConstructorMemberDeclaration modifiers c params (mkValidationsCtor ++ mkAssignments)
    where 
        params = mkFormalParamPs ps
        mkAssignments = map mkAssignmentP ps
        mkAssignmentP (Property n t _) = ExpressionStatement $ mkAssignThisDot n (camelCase n)
        
        mkValidationsCtor = if(applyNotNull) then catMaybes $ map mkValidationCtor ps else []
        mkValidationCtor (Property n t vals) = 
            if hasNotNullValidationsP vals 
            then Just $ mkValidationNotNull (camelCase n) t
            else Nothing

mkSimpleNameArgumentP :: Property -> Argument
mkSimpleNameArgumentP (Property n _ _) = mkSimpleNameArgument (camelCase n)

mkSimpleNameArgumentPs :: [Property] -> [Argument]
mkSimpleNameArgumentPs ps = map mkSimpleNameArgumentP ps

mkNewPs :: String -> [Property] -> Expression
mkNewPs c ps = mkNew c (mkSimpleNameArgumentPs ps)

mkPublicStaticCreateMethod :: String -> [Property] -> MemberDeclaration
mkPublicStaticCreateMethod c ps = 
    mkMethodMemberDeclarationPublicStatic
        (mkTypeNamed choiceT)
        "Create"
        (mkFormalParamPs ps) 
        (mkAllValidations ++  [mkReturnCtorOk])
    where
        choiceT = mkChoiceT c $ c ++ "Error"

        mkAllValidations = mkValidationsPs c ps
        mkReturnCtorOk = mkChoice1Of2Return choiceT [mkArgument callCtor]
        callCtor = mkNewPs c ps

mkValidationsPs :: String -> [Property] -> [Statement]
mkValidationsPs c ps = concat $ map (mkValidationsP c) ps

mkValidationsP :: String -> Property -> [Statement]
mkValidationsP c (Property n t vals) = map (mkValidationPVal c t (camelCase n)) vals

mkValidationNotNull :: String -> String -> Statement
mkValidationNotNull n t = ifNullThen n (mkThrowArgumentNullException n t)

mkValidationPVal :: String -> String -> String -> PropertyValidation -> Statement
mkValidationPVal _ t n (NotNull) = mkValidationNotNull n t
mkValidationPVal c _ n (MaxLength l) = ifLengthGreaterThanThen n l (returnError c errorName)
    where
        errorName = "MaxLength" ++ n ++ "Error"
        ifLengthGreaterThanThen = ifLengthBinaryOpThen BinaryGreaterThan
mkValidationPVal c _ n (MinLength l) = ifLengthLessThanThen n l (returnError c errorName)
    where
        errorName = "MinLength" ++ n ++ "Error"
        ifLengthLessThanThen = ifLengthBinaryOpThen  BinaryLessThan
mkValidationPVal c _ n (Required) = ifNullThen n (returnError c $ (capitalize n) ++ "FieldIsRequired")
mkValidationPVal c _ n1 (Equals n2) = ifNotEqualsThen (camelCase n2) n1 (returnError c $ n2 ++ "And"++ (capitalize n1) ++ "NotEquals")
    where
        ifNotEqualsThen n1 n2 = ifThen (UnaryNot (Invocation (MemberAccess (PrimaryMemberAccess (mkSimpleName n1) (Identifier "Equals") [])) [Argument Nothing (mkSimpleName n2)]))
mkValidationPVal c _ n1 (NotEquals n2) = ifEqualsThen (camelCase n2) n1 (returnError c $ n2 ++ "And"++ (capitalize n1) ++ "Equals")
    where
        ifEqualsThen n1 n2 = ifThen (Invocation (MemberAccess (PrimaryMemberAccess (mkSimpleName n1) (Identifier "Equals") [])) [Argument Nothing (mkSimpleName n2)])

returnError :: String -> String -> Statement
returnError c e = mkChoice2Of2Return choiceT [mkSimpleNameArgument error]
    where
        cError = c ++ "Error"
        choiceT = mkChoiceT c cError
        error = cError ++ "." ++ e

ifLengthBinaryOpThen :: BinaryOperator -> String -> Integer -> Statement -> Statement
ifLengthBinaryOpThen op n l = ifThenBinaryOp op (mkSimpleName (n++".Length")) (Literal $ IntLit l)
        