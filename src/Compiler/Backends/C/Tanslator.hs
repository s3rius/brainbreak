{-# LANGUAGE QuasiQuotes #-}

module Compiler.Backends.C.Tanslator where

import           Compiler.Backends.C.AST
import           Data.String.Interpolate

translateHeaders :: CHeader -> String
translateHeaders (CInclude name)        = [i|\#include <#{name}>|]
translateHeaders (CUsingNamespace name) = [i|using namespace #{name};|]

translateVar :: CVar -> String
translateVar (MapElement var1 var2) =
  [i|#{translateVar var1}[#{translateVar var2}]|]
translateVar var = _name var

translateType :: CType -> String
translateType CTypeInt = "int"
translateType (CTypeMap key val) =
  [i|map<#{translateType key},#{translateType val}>|]

translateValue :: CValue -> String
translateValue (Left var) = translateVar var
translateValue (Right (CConstInt value)) = show value
translateValue (Right (CConstString value)) = [i|"#{value}"|]
translateValue (Right (CEmptyMap key val)) =
  [i|map<#{translateType key}, #{translateType val}>()|]

translateOperations :: COperation -> String
translateOperations (CPrint var) = [i|printf("%c", #{translateVar var});|]
translateOperations (CRead var) = [i|#{translateVar var} = getchar();|]
translateOperations (CDeclare (MapElement m el)) =
  error "Can't declare element of map!"
translateOperations (CDeclare (CVar name var_type)) =
  [i|#{translateType var_type} #{name};|]
translateOperations (CSet var value) =
  [i|#{translateVar var} = #{translateValue value};|]
translateOperations (CAdd var1 var2 val) =
  [i|#{translateVar var1} = #{translateVar var2} + #{translateValue val};|]
translateOperations (CDecrease var1 var2 val) =
  [i|#{translateVar var1} = #{translateVar var2} - #{translateValue val};|]
translateOperations (CLoop var ops) =
  [i|
while (#{translateVar var}){
    #{unlines $ map (("\t" ++ ) . translateOperations) ops}
}
|]

translateC :: CModule -> String
translateC (CModule headers operations) =
  [i|
#{unlines $ map translateHeaders headers}

int main(){
#{unlines $ map translateOperations operations}
return 0;
}
|]
