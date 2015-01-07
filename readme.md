# borgmanities

_resistence is facile_

a nascent twitter bot

## The plan

At the moment, we're trying to write it in Haskell, probably using wordnet and
the twitter GET api. But we are being flexible. This is a learning
exercise.

## Feature Ideas

### Humanities paper titles ###

e.g.: "Xtreme Formulations: The Gendered Semantic Hierarchy of Male Programmers"

#### Alogrithm sketch (jw):

1. pick the first noun (`seedNoun`) that occurs in the most recent tweet in a set of `hashtags` we prepopulate
2. run it through the "has types" of the Words API
3. pick the second noun in that list—that is the anchor noun (`typeVariant`)
4. run the original/ 1st noun through API definitions
5. the first definition prior to any punctuation is the pre-colon phrase (`d1`).
6. the anchor noun is then pluralized / has "the" place in front of it
7. anchor noun is the first word after the colon
8. randomly select a preposition (`randPrep`)
9. place preposition after anchor word
10. search Twitter with search terms: [preposition selected in step 8] + first noun from step 1
11. take first noun (`compliment`)

...

##### Elaboration:

Title schematic:

```
[d1] : [typeVariant] [randPrep] [complement]
```

where

- `d1`          = the first clause prior to any punctuation or disjunction from the first definition of `seedNoun`
- `typeVariant` = a random element from hasType or typeof in the second definition of `seedNoun`
- `randPrep`    = random preposition
- `complement`  = noun phrase following the first tweet returned by
  searching "[randPrep] [seedNoun]"

hard coded resources:

- `hashtags`     = curated list of hashtag terms -> `randHashtag`
- `prepositions` = curated list of prepositions  -> `randPrep`

input resources:

- twitter search of `randHashtag`           -> `seedNoun`
- wordsapi search of `seedNoun`             -> `definitions`
- twitter search of `seedNoun` + `randPrep` -> `complement`

Algorithm:

1. Select `randHashtag` from `hashtags`
1. Select `seedNoun` from twitter search of `randHashtag`
1. Get wordsApi `definitions` of `seedNoun`
1. Select `d1` from `definitions`
1. Select random `typeVariant` from `definitions`
1. Pluralize or prepend "the" to `typeVariant`
1. `title = (titleCase . unwords) [d1 ++ ":", typeVariant, randPrep, complement]`
1. `msg = "About to publish an article: " ++ "\"" ++ title ++ "\"."`
1. `if charCount title < 141 then tweet msg else do (1)`

Self-awareness:

- Maintain list of past seed nouns, lest it repeat itself.
- Blacklist, lest it be an evil POS.

#### Examples:

##### 1

`seedNoun`    = "brick"
`typeVariant` = "firebrick"
`d1`          = "a good fellow"
`randPrep`    = "in"
`compliment`  = "the Mail"

result: "A Good Fellow: the Firebrick in the Mail"

##### 2

`randHashtag` = "#oppression"
`seedNoun`    = "membership"
`typeVariant` = "relationship"
`d1`          = "the body of members of an organization"
`randPrep`    = "through"
`compliment`  = "our health care"

result: "The Body of Members of an Organization: Relationships Through
Our Health Care"

### Research Topic Generator ###

e.g.: "post-augmentationism", "neo-retrofuturism"

### Academic Journal Name Generator ###

### Current Research Updates ###

e.g.: "I'm starting to become interested in
[generated research topic]", ""

## Resources to keep in mind

- http://developer.wordnik.com/
