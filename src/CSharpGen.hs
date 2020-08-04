module CSharpGen
    ( transform
    ) where

import Parsing
import Language.CSharp.Syntax
import Data.Char (toLower, toUpper)

transform :: DslVal -> Either String CompilationUnit
transform (Namespace ns cs) = Right $ CompilationUnit [] [mkNamespace ns (map transformClass cs)]

mkNamespace :: String -> [Declaration] -> Declaration
mkNamespace ns = NamespaceDeclaration [] (mkName ns)

transformClass :: Class -> Declaration
transformClass (Class c ps) = transformClassC c ps

transformClassC :: String -> [Property] -> Declaration
transformClassC c ps = mkPublicClass c (if hasValidationsPs then (generateStaticCreateMethod:ctorAndProperties) else ctorAndProperties)
    where 
        hasValidationsP (Property _ _ vals) = not (null vals)
        hasValidationsPs = or $ map hasValidationsP ps

        generateStaticCreateMethod = mkPublicStaticCreateMethod c ps

        ctorAndProperties = generateCtor : generateProperties
        generateCtor = mkCtor [if(hasValidationsPs) then Private else Public] c ps
        generateProperties = createProperties ps

        
createProperties :: [Property] -> [MemberDeclaration]
createProperties ps = map mkAutoProperty ps
    where 
        mkAutoProperty (Property n t _) = mkPropertyAutoPublicGet t n


mkPrivateCtor :: String -> [Property] -> MemberDeclaration
mkPrivateCtor c ps = mkCtor [Private] c ps

mkCtor :: [Modifier] -> String -> [Property] -> MemberDeclaration
mkCtor m c ps = 
    ConstructorMemberDeclaration 
        [] 
        m 
        (Identifier c) 
        (FormalParams (mkCreateMethodParams ps) (Nothing)) 
        Nothing 
        (mkPrivateCtorBody ps)
    where 
        mkFormalParamP (Property n t _) = mkFormalParam t (camelCase n)
        mkCreateMethodParams ps = map mkFormalParamP ps
        mkPrivateCtorBody ps = ConstructorStatementBody (map mkPrivateCtorBodyP ps)
        mkPrivateCtorBodyP (Property n _ _) = ExpressionStatement $ mkAssignThisDot n (camelCase n)

camelCase :: String -> String
camelCase (head:tail) = toLower head : tail
camelCase [] = []

        
mkPropertyAutoPublicGet :: String -> String -> MemberDeclaration
mkPropertyAutoPublicGet t n = PropertyMemberDeclaration [] [Public] (mkTypeNamed t) (mkName n) mkAutoPropertyBody
    where
        mkAutoPropertyBody = PropertyBody mkGetAccessorDeclarationAuto mkSetAccessorDeclarationAutoPrivate Nothing
        mkGetAccessorDeclarationAuto = Just $ GetAccessorDeclaration [] [] Nothing
        mkSetAccessorDeclarationAutoPrivate = Just $ SetAccessorDeclaration [] [Private] Nothing

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
        mkValidations (Property n t vals) = map (mkValidation (camelCase n)) vals

        ifNullThen n = ifThenBinaryOp BinaryEquals (mkSimpleName n) (Literal NullLit)

        ifLengthBinaryOpThen op n l = ifThenBinaryOp op (mkSimpleName (n++".Length")) (Literal $ IntLit l)
        ifLengthGreaterThanThen = ifLengthBinaryOpThen BinaryGreaterThan
        ifLengthLessThanThen = ifLengthBinaryOpThen  BinaryLessThan

        returnError e = (mkReturn (choice2Of2 [mkSimpleNameArgument $ cError ++ "." ++ e]))
        mkValidation n (NotNull) = ifNullThen n (Throw (Just $ mkNew "System.ArgumentNullException" [mkLiteralStringArgument n])) 
        mkValidation n (MaxLength l) = ifLengthGreaterThanThen n l (returnError $ "MaxLength" ++ n ++ "Error")
        mkValidation n (MinLength l) = ifLengthLessThanThen n l (returnError $ "MinLength" ++ n ++ "Error")




mkAssignThisDot :: String -> String -> Expression
mkAssignThisDot l r = mkAssign (MemberAccess $ mkPrimaryMemberAccessThisDot l) (mkSimpleName r)

mkArgument :: Expression -> Argument
mkArgument = Argument Nothing

mkSimpleNameArgument :: String -> Argument
mkSimpleNameArgument n = mkArgument (mkSimpleName n)

mkLiteralStringArgument :: String -> Argument
mkLiteralStringArgument s = mkArgument (Literal (StringLit s))

mkFormalParam :: String -> String -> FormalParam
mkFormalParam t n = FormalParam (Nothing) (mkTypeNamed t) (Identifier n) (Nothing)

mkPublicClass :: String -> [MemberDeclaration] -> Declaration
mkPublicClass className cb = TypeDeclaration $ ClassTypeDeclaration [] [Public, Partial] (Identifier className) [] [] [] (ClassBody cb)

mkTypeNamed :: String -> Type
mkTypeNamed t = (TypeNamed (TypeName (mkName t) []))

mkName :: String -> Name
mkName n = (Name [Identifier n])

-- (Assign (MemberAccess (PrimaryMemberAccess This (Identifier "IsLabelChooseByServer") [])) OpAssign (SimpleName (Identifier "IsLabelChooseByServer") []))

mkPrimaryMemberAccess :: Expression -> String -> MemberAccess
mkPrimaryMemberAccess obj p = PrimaryMemberAccess obj (Identifier p) []
mkPrimaryMemberAccessThisDot :: String -> MemberAccess
mkPrimaryMemberAccessThisDot p = PrimaryMemberAccess This (Identifier p) []

mkAssign :: Expression -> Expression -> Expression
mkAssign l r = Assign l OpAssign r

mkNew :: String -> [Argument] -> Expression
mkNew cn args = ObjectCreationExpression (mkTypeNamed cn) args Nothing

mkSimpleName :: String -> Expression
mkSimpleName n = SimpleName (Identifier n) []

ifThenBinaryOp :: BinaryOperator -> Expression -> Expression -> Statement -> Statement
ifThenBinaryOp ifOp if1 if2 thenOp = ifThen (BinaryOperator ifOp if1 if2) thenOp

ifThen :: Expression -> Statement -> Statement
ifThen ifOp thenOp = IfThenElse
                ifOp 
                thenOp
                Nothing

mkReturn :: Expression -> Statement
mkReturn exp = Return $ Just exp

