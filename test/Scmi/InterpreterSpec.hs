module Scmi.InterpreterSpec where

import Test.Hspec

import Scmi.Interpreter

spec :: Spec
spec =
    
    describe "expression showing" $ do

        let shouldShow = shouldBe . show
        
        it "shows number" $ do
            Number 42 `shouldShow`
                "42"

        it "shows boolean" $ do
            Boolean False `shouldShow`
                "#f"
            Boolean True `shouldShow`
                "#t"

        it "shows empty list" $ do
            EmptyList `shouldShow`
                "()"

        it "shows unspecified value" $ do
            Unspecified `shouldShow`
                "#<void>"

        it "shows variable" $ do
            Var (Ident "scm") `shouldShow`
                "scm"

        it "shows quotation" $ do
            Quotation (Number 42) `shouldShow`
                "(quote 42)"

        it "shows assignment" $ do
            Assignment (Ident "x") (Boolean False) `shouldShow`
                "(set! x #f)"

        it "shows definition" $ do
            Definition (Ident "y") (Number 73) `shouldShow`
                "(define y 73)"

        it "shows conditional" $ do
            Conditional (Boolean True) (Number 1) (Number 0) `shouldShow`
                "(if #t 1 0)"

        it "shows lambda" $ do
            Lambda [Ident "x", Ident "y"] (Var (Ident "x")) `shouldShow`
                "(lambda (x y) x)"

        it "shows sequence" $ do
            Sequence [Number 73, Quotation (Var (Ident "z"))] `shouldShow`
                "(begin 73 (quote z))"

        it "shows application" $ do
            Application (Var (Ident "+")) [Number 2, Number 3, Number 5] `shouldShow`
                "(+ 2 3 5)"
