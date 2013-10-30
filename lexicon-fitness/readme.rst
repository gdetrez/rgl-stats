lexicon-test
############

This is a script that can be used to test a fullform lexicon against an
external source. It compute both coverage and fitness of the full form lexicon
given an reference wordlist.

Coverage
========

Coverage is defined as the percentage of words of the wordlist which are
in the lexicon.

Fitness
=======

Fitness of a lexicon entry is the percentage of its forms that can be found in
the wordlist. One can see the wordlist as a simple spellchecker and a form which
cannot be found in the wordlist is considered incorrect.

The global fitness figure for the lexicon is just the mean for all entries.

I use the word fitness and not correctness because the later sound a bit too
strong and can hide the fact that this figure is highly dependent on the given
wordlist.

Wordlists
=========

In this context, a wordlist is a text file were each line contains a single
word form. It may be generated from a corpus or from an other lexicon.

From a lexicon
--------------
Given a lexicon in the format accepted by this program (csv without spaces),
it's easy to create a wordlist:

    tr ',' '\n' < lexicon.csv

This is mostly useful for testing purposes

From Lefff
----------

Generating a wordlist for a category (for instance v for verbs):

    awk '/^[^#]/ { if ($2 == "v") print $1}' < lefff-2.1.txt | iconv -f LATIN1 -t UTF-8

From Morphalou
--------------

Morphalou is in some xml format, we can use xmlstarlet to extract data from it:

    xmlstarlet sel -T -t -m '//lexicalEntry/formSet' \
                      -i 'lemmatizedForm/grammaticalCategory = "verb"' \
                      -m 'inflectedForm/orthography' \
                      -v '.' -n test.xml

