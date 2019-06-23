X
=

Light-weight Haskell XML library derived from the
[`xml` package](http://hackage.haskell.org/package/xml)
for simple parsing and creation of XML documents.
It only depends on `base`, `bytestring`, `text`, and `text-short`.

Example
-------

```
{-# LANGUAGE RecordWildCards #-}
import Text.XML

data Package = Package
  { pOrderNo  :: Text
  , pOrderPos :: Text
  , pBarcode  :: Text
  , pNumber   :: Text
  }

-- | Create XML from a Package
instance Node Package where
  node qn Package {..} =
    node qn
      [ unode "package_number" pNumber
      , unode "package_barcode" pBarcode
      , unode "order_number" pOrderNo
      , unode "order_position" pOrderPos
      ]
```
