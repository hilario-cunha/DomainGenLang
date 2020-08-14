module CSharpGen
    ( transform
    ) where

import Parsing
import Language.CSharp.Syntax
import Data.Char (toLower, toUpper)

transform :: DslVal -> Either String CompilationUnit
transform (Namespace usings ns cs) = Right $ CompilationUnit (mkUsings usings) [mkNamespace ns (map transformClass cs)]

mkUsings :: [String] -> [Using]
mkUsings = map mkUsing

mkUsing :: String -> Using
mkUsing u = Using (mkName u) False

mkNamespace :: String -> [Declaration] -> Declaration
mkNamespace ns = NamespaceDeclaration [] (mkName ns)

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
        mkPrivateCtorBody = ConstructorStatementBody (concat (map mkPrivateCtorBodyP ps))

        mkFormalParamP (Property n t _) = mkFormalParam t (camelCase n)
        mkPrivateCtorBodyP (Property n t vals) = if(applyNotNull && hasNotNullValidations vals) then [mkValidation, assignStatement] else [assignStatement]
            where 
                hasNotNullValidations vals = not $ null $ filter (==NotNull) vals
                ifNullThen n = ifThenBinaryOp BinaryEquals (mkSimpleName $ camelCase n) (Literal NullLit)
                mkValidation = ifNullThen n (Throw (Just $ mkNew "System.ArgumentNullException" [mkLiteralStringArgument $ camelCase n, mkLiteralStringArgument $ "Field "++ camelCase n ++" with type "++ t ++ " can not be null"])) 
                assignStatement = ExpressionStatement $ mkAssignThisDot n (camelCase n)


camelCase :: String -> String
camelCase (head:tail) = toLower head : tail
camelCase [] = []

capitalize :: String -> String
capitalize (head:tail) = toUpper head : tail
capitalize [] = []

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

mkAutoGetAccessorDeclaration :: [Modifier] -> Maybe AccessorDeclaration
mkAutoGetAccessorDeclaration modifiers = Just $ GetAccessorDeclaration [] modifiers Nothing

mkAutoSetAccessorDeclaration :: [Modifier] -> Maybe AccessorDeclaration
mkAutoSetAccessorDeclaration modifiers = Just $ SetAccessorDeclaration [] modifiers Nothing

mkPropertyBody :: Maybe AccessorDeclaration -> Maybe AccessorDeclaration -> PropertyBody
mkPropertyBody getAccessorDeclaration setAccessorDeclaration = PropertyBody getAccessorDeclaration setAccessorDeclaration Nothing

mkPropertyMemberDeclaration :: [Modifier] -> Type -> Name -> PropertyBody -> MemberDeclaration
mkPropertyMemberDeclaration modifiers _type name body = PropertyMemberDeclaration [] modifiers _type name body

mkAutoPropertyBodyPrivateSet :: PropertyBody
mkAutoPropertyBodyPrivateSet = mkPropertyBody (mkAutoGetAccessorDeclaration []) (mkAutoSetAccessorDeclaration [Private])

mkPropertyAutoPublicGet :: String -> String -> MemberDeclaration
mkPropertyAutoPublicGet t n = mkPropertyMemberDeclaration [Public] (mkTypeNamed t) (mkName n) (mkAutoPropertyBodyPrivateSet)

