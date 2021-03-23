module Rel8.Type.JSONBEncoded ( JSONBEncoded(..) ) where

-- aeson
import Data.Aeson ( FromJSON, ToJSON, parseJSON, toJSON )
import Data.Aeson.Types ( parseEither )

-- base
import Data.Bifunctor ( first )
import Prelude

-- hasql
import qualified Hasql.Decoders as Hasql

-- rel8
import Rel8.Type ( DBType(..) )
import Rel8.Type.Information ( TypeInformation(..) )

-- text
import Data.Text ( pack )


-- | Like 'JSONEncoded', but works for @jsonb@ columns.
newtype JSONBEncoded a = JSONBEncoded { fromJSONBEncoded :: a }


instance (FromJSON a, ToJSON a) => DBType (JSONBEncoded a) where
  typeInformation = TypeInformation
    { encode = encode typeInformation . toJSON . fromJSONBEncoded
    , decode = Hasql.refine (first pack . fmap JSONBEncoded . parseEither parseJSON) Hasql.jsonb
    , typeName = "jsonb"
    , out = id
    }
