# tulipac: The TuLiPA compiler

[TuLiPA](https://sourcesup.cru.fr/tulipa/) is the Tübingen Parsing Architecture. 
It is an excellent parser
for teaching [tree-adjoining grammar](http://en.wikipedia.org/wiki/Tree-adjoining_grammar),
except that it assumes an XML
format for TAG grammars that is really complicated to work
with. 

Tulipac, the TuLiPA grammar compiler, fixes this problem by
providing a front-end that compiles TAG grammars in a human-readable
format to the XML format assumed by TuLiPA.

**Note:** You can now directly parse TAG grammars in the tulipac format with [Alto](https://github.com/coli-saar/alto/wiki/Parsing-TAG-with-feature-structures-%28tulipac%29). This will probably be faster than parsing with TuLiPA directly.
