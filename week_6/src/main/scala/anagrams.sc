import forcomp.Anagrams._

def wordAnagrams(occurrences: Occurrences): List[Word] =
  dictionaryByOccurrences(occurrences)

val sentence: Sentence = List("yes", "man")

def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
  def loop(occurrences: Occurrences): List[Sentence] = {
    if (occurrences.isEmpty) List(List())
    else combinations(occurrences).flatMap(wordAnagrams) match {
      case words => words.flatMap(word => loop(subtract(occurrences, wordOccurrences(word))).map(word :: _))
      case _ => List()
    }
  }

  val sOccurrences = sentenceOccurrences(sentence)
  loop(sOccurrences)
}

sentenceAnagrams(sentence).size