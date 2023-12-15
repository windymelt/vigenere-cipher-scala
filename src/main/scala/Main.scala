// ヴィジュネル暗号をScalaで実装する

val table = ('A' to 'Z').zipWithIndex.toMap
val elbat = table.map(_.swap)

def encrypt(key: String, plain: String): String =
  // 大文字に揃える
  val plainUpper = plain.toUpperCase()
  val k0 = LazyList.from(key.toUpperCase())
  val keyUpper: LazyList[Char] = LazyList.continually(k0).flatten // infinitely repeat `k0`

  // kとpをzipし、それぞれの文字を数値に変換して足し合わせてmod 26を取る
  val cipher = (keyUpper zip plainUpper).map: (k, p) =>
    (table(k) + table(p)) % 26
  cipher.map(elbat(_)).mkString

@main def vigenere(
    key: String = "ephemera",
    plain: String = "allyourbasearebelongtous"
): Unit =
  println(s"Vigenere cipher for [$plain] using key [$key]")
  println(encrypt(key, plain))
