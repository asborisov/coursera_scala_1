import forcomp.Anagrams.{Occurrences, Word, combinations, dictionary, wordOccurrences}
import forcomp.loadDictionary

import scala.annotation.tailrec

val occurrences: Occurrences = wordOccurrences("koko")

combinations(occurrences)
