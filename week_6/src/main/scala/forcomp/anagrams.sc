import forcomp.Anagrams.{Occurrences, Word, combinations, dictionary, wordOccurrences}

val occurrences: Occurrences = wordOccurrences("kokok")

occurrences.flatMap(p => for (n <- 1 to p._2) yield (p._1, n))
  .toSet[(Char, Int)].subsets
  .filter(p => p.groupBy(l => l._1).forall(p => p._2.size == 1))
  .map(_.toList.sortBy(_._1))
  .toList.size