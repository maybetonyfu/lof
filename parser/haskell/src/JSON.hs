{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module JSON where

import Data.Aeson
import Language.Haskell.Exts
import Language.Haskell.Names
import RIO.Vector (cons)

-- import Data.Vector
instance ToJSON (NameInfo SrcSpanInfo) where
  toJSON (GlobalSymbol a b) =
    object
      [ "type" .= Data.Aeson.String "GlobalSymbol",
        "Symbol" .= a,
        "QName" .= b
      ]
  toJSON (LocalValue a) =
    object
      [ "type" .= Data.Aeson.String "LocalValue",
        "loc" .= a
      ]
  toJSON (TypeVar a) =
    object
      [ "type" .= Data.Aeson.String "TypeVar",
        "loc" .= a
      ]
  toJSON (ValueBinder) =
    object
      [ "type" .= Data.Aeson.String "ValueBinder"
      ]
  toJSON (TypeBinder) =
    object
      [ "type" .= Data.Aeson.String "TypeBinder"
      ]
  toJSON (Import a) =
    object
      [ "type" .= Data.Aeson.String "Import"
      ]
  toJSON (ImportPart a) =
    object
      [ "type" .= Data.Aeson.String "ImportPart"
      ]
  toJSON (Export a) =
    object
      [ "type" .= Data.Aeson.String "Export"
      ]
  toJSON (RecPatWildcard a) =
    object
      [ "type" .= Data.Aeson.String "RecPatWildcard"
      ]
  toJSON (RecExpWildcard a) =
    object
      [ "type" .= Data.Aeson.String "RecExpWildcard"
      ]
  toJSON (None) = Null
  toJSON (ScopeError a) =
    object
      [ "type" .= Data.Aeson.String "Error"
      ]

instance ToJSON (QName ())

instance ToJSON (ModuleName ())

instance ToJSON (Name ())

instance ToJSON (SpecialCon ())

instance ToJSON (Scoped SrcSpanInfo) where
  toJSON (Scoped n l) =
    object
      [ "loc" .= l,
        "scope" .= n
      ]

instance ToJSON SrcSpanInfo where
  toJSON (SrcSpanInfo a b) = toJSON a

instance ToJSON SrcSpan where
  toJSON (SrcSpan f fromL fromC toL toC) =
    object
      [ "type" .= Data.Aeson.String "span",
        "file" .= f,
        "from"
          .= object
            [ "line" .= fromL,
              "col" .= fromC
            ],
        "to"
          .= object
            [ "line" .= toL,
              "col" .= toC
            ]
      ]

instance ToJSON Tool

instance ToJSON SrcLoc where
  toJSON (SrcLoc f line col) =
    object
      [ "type" .= Data.Aeson.String "point",
        "line" .= line,
        "col" .= col
      ]

instance ToJSON (ModulePragma (Scoped SrcSpanInfo))

instance ToJSON (Annotation (Scoped SrcSpanInfo))

instance ToJSON (Module (Scoped SrcSpanInfo))

instance ToJSON (ModuleHead (Scoped SrcSpanInfo))

instance ToJSON (WarningText (Scoped SrcSpanInfo))

instance ToJSON (ExportSpecList (Scoped SrcSpanInfo))

instance ToJSON (ExportSpec (Scoped SrcSpanInfo))

instance ToJSON (EWildcard (Scoped SrcSpanInfo))

instance ToJSON (ImportDecl (Scoped SrcSpanInfo))

instance ToJSON (ImportSpecList (Scoped SrcSpanInfo))

instance ToJSON (ImportSpec (Scoped SrcSpanInfo))

instance ToJSON (Assoc (Scoped SrcSpanInfo))

instance ToJSON (Namespace (Scoped SrcSpanInfo))

instance ToJSON (Decl (Scoped SrcSpanInfo))

instance ToJSON (Type (Scoped SrcSpanInfo))

instance ToJSON (Splice (Scoped SrcSpanInfo))

instance ToJSON (Overlap (Scoped SrcSpanInfo))

instance ToJSON (CallConv (Scoped SrcSpanInfo))

instance ToJSON (Safety (Scoped SrcSpanInfo))

instance ToJSON (Rule (Scoped SrcSpanInfo))

instance ToJSON (RuleVar (Scoped SrcSpanInfo))

instance ToJSON (Activation (Scoped SrcSpanInfo))

instance ToJSON (BooleanFormula (Scoped SrcSpanInfo))

instance ToJSON (DeclHead (Scoped SrcSpanInfo))

instance ToJSON (InstRule (Scoped SrcSpanInfo))

instance ToJSON (InstHead (Scoped SrcSpanInfo))

instance ToJSON (Binds (Scoped SrcSpanInfo))

instance ToJSON (IPBind (Scoped SrcSpanInfo))

instance ToJSON (PatternSynDirection (Scoped SrcSpanInfo))

instance ToJSON (InjectivityInfo (Scoped SrcSpanInfo))

instance ToJSON (ResultSig (Scoped SrcSpanInfo))

instance ToJSON (ClassDecl (Scoped SrcSpanInfo))

instance ToJSON (InstDecl (Scoped SrcSpanInfo))

instance ToJSON (Deriving (Scoped SrcSpanInfo))

instance ToJSON (DerivStrategy (Scoped SrcSpanInfo))

instance ToJSON (DataOrNew (Scoped SrcSpanInfo))

instance ToJSON (ConDecl (Scoped SrcSpanInfo))

instance ToJSON (FieldDecl (Scoped SrcSpanInfo))

instance ToJSON (QualConDecl (Scoped SrcSpanInfo))

instance ToJSON (GadtDecl (Scoped SrcSpanInfo))

instance ToJSON (BangType (Scoped SrcSpanInfo))

instance ToJSON (Unpackedness (Scoped SrcSpanInfo))

instance ToJSON (Match (Scoped SrcSpanInfo))

instance ToJSON (Rhs (Scoped SrcSpanInfo))

instance ToJSON (GuardedRhs (Scoped SrcSpanInfo))

instance ToJSON (Context (Scoped SrcSpanInfo))

instance ToJSON (FunDep (Scoped SrcSpanInfo))

instance ToJSON (Asst (Scoped SrcSpanInfo))

instance ToJSON (TyVarBind (Scoped SrcSpanInfo))

instance ToJSON (Promoted (Scoped SrcSpanInfo))

instance ToJSON (TypeEqn (Scoped SrcSpanInfo))

instance ToJSON (Exp (Scoped SrcSpanInfo))

instance ToJSON (Bracket (Scoped SrcSpanInfo))

instance ToJSON (Stmt (Scoped SrcSpanInfo))

instance ToJSON (QualStmt (Scoped SrcSpanInfo))

instance ToJSON (FieldUpdate (Scoped SrcSpanInfo))

instance ToJSON (Alt (Scoped SrcSpanInfo))

instance ToJSON (XAttr (Scoped SrcSpanInfo))

instance ToJSON (Pat (Scoped SrcSpanInfo))

instance ToJSON (PatField (Scoped SrcSpanInfo))

instance ToJSON (PXAttr (Scoped SrcSpanInfo))

instance ToJSON (RPat (Scoped SrcSpanInfo))

instance ToJSON (RPatOp (Scoped SrcSpanInfo))

instance ToJSON (Literal (Scoped SrcSpanInfo))

instance ToJSON (Sign (Scoped SrcSpanInfo))

instance ToJSON (ModuleName (Scoped SrcSpanInfo))

instance ToJSON (QName (Scoped SrcSpanInfo))

instance ToJSON (Name (Scoped SrcSpanInfo))

instance ToJSON (QOp (Scoped SrcSpanInfo))

instance ToJSON (Op (Scoped SrcSpanInfo))

instance ToJSON (SpecialCon (Scoped SrcSpanInfo))

instance ToJSON Boxed

instance ToJSON (CName (Scoped SrcSpanInfo))

instance ToJSON (IPName (Scoped SrcSpanInfo))

instance ToJSON (XName (Scoped SrcSpanInfo))

instance ToJSON (Role (Scoped SrcSpanInfo))

instance ToJSON (MaybePromotedName (Scoped SrcSpanInfo))
