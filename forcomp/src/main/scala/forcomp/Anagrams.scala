package forcomp

import common._

object Anagrams {
  val stream = new java.io.ByteArrayOutputStream()

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers
    * saying how often the character appears. This list is sorted alphabetically
    * w.r.t. to the character in each pair. All characters in the occurrence
    * list are lowercase.
    *
    * Any list of pairs of lowercase characters and their frequency which is not
    * sorted is **not** an occurrence list.
    *
    * Note: If the frequency of some character is zero, then that character
    * should not be in the list.
    */
  type Occurrences = List[(Char, Int)]

  val emptyOccurrence: Occurrences = List[(Char, Int)]()

  /** The dictionary is simply a sequence of words. It is predefined and
    * obtained as a sequence using the utility method `loadDictionary`.
    */
  val dictionary: List[Word] = loadDictionary

  /** Merges two occurrence lists into a occurrence list that has character
    * count of addition of them.
    */
  def mergeOccurrences(fst: Occurrences, snd: Occurrences): Occurrences = {
    val merged = (fst ++ snd).groupBy(_._1).mapValues(_.foldLeft(0)(_ + _._2))
    merged.toList.sortBy(_._1)
  }

  /** Converts the word into its character occurrence list.
    *
    * Note: the uppercase and lowercase version of the character are treated as
    * the same character, and are represented as a lowercase character in the
    * occurrence list.
    */
  def wordOccurrences(word: Word): Occurrences =
    word.groupBy(_.toLower).mapValues(_.length).toList.sortBy(_._1)

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(sentence: Sentence): Occurrences =
    sentence.foldLeft(emptyOccurrence)((accum, word) =>
      mergeOccurrences(accum, wordOccurrences(word))
    )

  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a
    * sequence of all the words that have that occurrence count. This map serves
    * as an easy way to obtain all the anagrams of a word given its occurrence
    * list.
    *
    * For example, the word "eat" has the following character occurrence list:
    *
    * `List(('a', 1), ('e', 1), ('t', 1))`
    *
    * Incidentally, so do the words "ate" and "tea".
    *
    * This means that the `dictionaryByOccurrences` map will contain an entry:
    *
    * List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
    */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] =
    dictionary.groupBy(wordOccurrences)

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] =
    dictionaryByOccurrences(wordOccurrences(word))

  /** Returns the power set of given occurrence.
    *
    * Example: powerOccurrence(('a', 3)) = List(List(), List(('a', 1)),
    * List(('a', 2)), List(('a', 3))
    */
  def powerOccurrence(occurrence: (Char, Int)): List[Occurrences] = {
    val (char, count) = occurrence

    @scala.annotation.tailrec
    def loop(accum: List[Occurrences], remaining: Int): List[Occurrences] =
      remaining match {
        case 0 => List[(Char, Int)]() :: accum
        case _ => loop(List((char, remaining)) :: accum, remaining - 1)
      }

    loop(Nil, count)
  }

  /** Computes Cartesian product of two Lists. */
  def cartesian(
      fsts: List[Occurrences],
      snds: List[Occurrences]
  ): List[Occurrences] =
    for { fst <- fsts; snd <- snds } yield fst ++ snd

  /** Returns the list of all subsets of the occurrence list. This includes the
    * occurrence itself, i.e. `List(('k', 1), ('o', 1))` is a subset of
    * `List(('k', 1), ('o', 1))`. It also include the empty subset `List()`.
    *
    * Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))`
    * are:
    *
    * List( List(), List(('a', 1)), List(('a', 2)), List(('b', 1)), List(('a',
    * 1), ('b', 1)), List(('a', 2), ('b', 1)), List(('b', 2)), List(('a', 1),
    * ('b', 2)), List(('a', 2), ('b', 2)) )
    *
    * Note that the order of the occurrence list subsets does not matter -- the
    * subsets in the example above could have been displayed in some other
    * order.
    */
  def combinations(occurrences: Occurrences): List[Occurrences] =
    occurrences.foldLeft(List(emptyOccurrence))((accum, occurrence) =>
      cartesian(accum, powerOccurrence(occurrence))
    )

  /** Subtracts occurrence list `snd` from occurrence list `fst`.
    *
    * The precondition is that the occurrence list `snd` is a subset of the
    * occurrence list `fst` -- any character appearing in `snd` must appear in
    * `fst`, and its frequency in `snd` must be smaller or equal than its
    * frequency in `fst`.
    *
    * Note: the resulting value is an occurrence - meaning it is sorted and has
    * no zero-entries.
    */
  def subtract(fst: Occurrences, snd: Occurrences): Occurrences = {
    snd.foldLeft(fst)((accum, occurrence) => {
      val (char, count) = occurrence
      val index = accum.indexWhere(_._1 == char)
      val (_, original) = accum(index)

      if (original == count)
        accum.patch(index, Nil, 1)
      else
        accum.updated(index, (char, original - count))
    })
  }

  /** Build anagrams by occurrences. */
  def occurrenceAnagrams(occurs: Occurrences): List[Sentence] = {
    val combs = combinations(occurs)
    def dict(occurs: Occurrences) =
      dictionaryByOccurrences.getOrElse(occurs, Nil)

    val sentenceLists = for (comb <- combs; word <- dict(comb)) yield {
      val remaining = subtract(occurs, wordOccurrences(word))
      val sentences = occurrenceAnagrams(remaining)
      sentences.map(_ :+ word)
    }

    if (!sentenceLists.isEmpty)
      sentenceLists.foldLeft(List[Sentence]())((accum, li) => accum ++ li)
    else if (occurs.isEmpty)
      List[Sentence](Nil)
    else
      Nil
  }

  /** Returns a list of all anagram sentences of the given sentence.
    *
    * An anagram of a sentence is formed by taking the occurrences of all the
    * characters of all the words in the sentence, and producing all possible
    * combinations of words with those characters, such that the words have to
    * be from the dictionary.
    *
    * The number of words in the sentence and its anagrams does not have to
    * correspond. For example, the sentence `List("I", "love", "you")` is an
    * anagram of the sentence `List("You", "olive")`.
    *
    * Also, two sentences with the same words but in a different order are
    * considered two different anagrams. For example, sentences `List("You",
    * "olive")` and `List("olive", "you")` are different anagrams of `List("I",
    * "love", "you")`.
    *
    * Here is a full example of a sentence `List("Yes", "man")` and its anagrams
    * for our dictionary:
    *
    * List( List(en, as, my), List(en, my, as), List(man, yes), List(men, say),
    * List(as, en, my), List(as, my, en), List(sane, my), List(Sean, my),
    * List(my, en, as), List(my, as, en), List(my, sane), List(my, Sean),
    * List(say, men), List(yes, man) )
    *
    * The different sentences do not have to be output in the order shown above
    * \- any order is fine as long as all the anagrams are there. Every returned
    * word has to exist in the dictionary.
    *
    * Note: in case that the words of the sentence are in the dictionary, then
    * the sentence is the anagram of itself, so it has to be returned in this
    * list.
    *
    * Note: There is only one anagram of an empty sentence.
    */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    val occurs = sentenceOccurrences(sentence)
    occurrenceAnagrams(occurs)
  }

}