import forcomp.Anagrams._

val sentence: Sentence = List("Ye", "ma")
def loop(occurrences: Occurrences): List[Sentence] = {
  if (occurrences.isEmpty) Nil
  else {
    val words = combinations(occurrences).flatMap(wordAnagrams)
    if (words.isEmpty) List()
    else words
        .flatMap(word => {
          // chars are left
          val rest = subtract(occurrences, wordOccurrences(word))
          // sentences can be created with rest chars
          val restSent: List[Sentence] = loop(rest)
          // add current word for each sentence
          restSent.map(s => word :: s)
        })
  }
}

val sOccurrences = sentenceOccurrences(sentence)
loop(sOccurrences) //.filter(s => s.forall(w => !w.contains("#")))