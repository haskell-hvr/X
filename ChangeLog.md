See also http://pvp.haskell.org/faq

### 0.3.1.0

- New `xmlns_elem_wellformed` predicate for verifying namespace-wellformedness
- New `qnameToText` and `qnameFromText` conversion functions
- Explicitly declare SafeHaskell levels; ensure all public modules are "Safe"
- Fix pure exception bypassing proper failure reporting
- Fix attribute uniqueness violations not being detected

## 0.3.0.0

- New `SerializeXMLOptions(serializeSortAttributes)` option
- Add new `Serializexmlroot
- Major refactoring of `Text.XML.Cursor` API
- un-`Maybe` the `QName(qURI)` field
