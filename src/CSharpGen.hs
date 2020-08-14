module CSharpGen where

import Language.CSharp.Syntax
import Data.Char (toLower, toUpper)

mkUsings :: [String] -> [Using]
mkUsings = map mkUsing

mkUsing :: String -> Using
mkUsing u = Using (mkName u) False

mkNamespace :: String -> [Declaration] -> Declaration
mkNamespace ns = NamespaceDeclaration [] (mkName ns)

camelCase :: String -> String
camelCase (head:tail) = toLower head : tail
camelCase [] = []

capitalize :: String -> String
capitalize (head:tail) = toUpper head : tail
capitalize [] = []

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
