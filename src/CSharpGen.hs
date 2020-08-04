module CSharpGen
    ( transform
    ) where

import Parsing
import Language.CSharp.Syntax

transform :: DslVal -> Either String CompilationUnit
transform (Class c ps) = Right $ CompilationUnit [] [transformClass c ps]

transformClass :: String -> [Property] -> Declaration
transformClass c ps = mkPublicClass c ((mkPublicStaticCreateMethod c ps):(mkPrivateCtor c ps):(map mkAutoProperty ps))

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

        mkFormalParamP (Property n t _) = mkFormalParam t n
        mkSimpleNameArgumentP (Property n _ _) = mkSimpleNameArgument n
        mkValidations (Property n t vals) = map (mkValidation n) vals

        ifNullThen n = ifThenBinaryOp BinaryEquals (mkSimpleName n) (Literal NullLit)

        ifLengthBinaryOpThen op n l = ifThenBinaryOp op (mkSimpleName (n++".Length")) (Literal $ IntLit l)
        ifLengthGreaterThanThen = ifLengthBinaryOpThen BinaryGreaterThan
        ifLengthLessThanThen = ifLengthBinaryOpThen  BinaryLessThan

        returnError e = (mkReturn (choice2Of2 [mkSimpleNameArgument $ cError ++ "." ++ e]))
        mkValidation n (NotNull) = ifNullThen n (Throw (Just $ mkNew "System.ArgumentNullException" [mkLiteralStringArgument n])) 
        mkValidation n (MaxLength l) = ifLengthGreaterThanThen n l (returnError $ "MaxLength" ++ n ++ "Error")
        mkValidation n (MinLength l) = ifLengthLessThanThen n l (returnError $ "MinLength" ++ n ++ "Error")

mkPrivateCtor :: String -> [Property] -> MemberDeclaration
mkPrivateCtor c ps = 
    ConstructorMemberDeclaration 
        [] 
        [Private] 
        (Identifier c) 
        (FormalParams (mkCreateMethodParams ps) (Nothing)) 
        Nothing 
        (mkPrivateCtorBody ps)
    where 
        mkFormalParamP (Property n t _) = mkFormalParam t n
        mkCreateMethodParams ps = map mkFormalParamP ps
        mkPrivateCtorBody ps = ConstructorStatementBody (map mkPrivateCtorBodyP ps)
        mkPrivateCtorBodyP (Property n _ _) = mkAssignStatement ("this."++n) n

mkAutoProperty :: Property -> MemberDeclaration
mkAutoProperty (Property n t _) = PropertyMemberDeclaration [] [Public] (mkTypeNamed t) (mkName n) mkAutoPropertyBody
    where
        mkAutoPropertyBody = 
            PropertyBody 
                (Just $ GetAccessorDeclaration [] [] Nothing) 
                (Just $ SetAccessorDeclaration [] [Private] Nothing) 
                (Nothing)



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

mkAssign :: String -> String -> Expression
mkAssign l r = Assign (mkSimpleName l) OpAssign (mkSimpleName r)

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

mkAssignStatement :: String -> String -> Statement
mkAssignStatement l r = ExpressionStatement $ mkAssign l r

