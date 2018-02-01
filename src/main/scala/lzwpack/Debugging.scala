package lzwpack

trait Debugging {
  case class Tag(name: String)

  private def summary(tag: String)(messages: Seq[String]): String =
    s"[$tag]" + (messages mkString "\t\t")

  def debug(messages: String*)(implicit tag: Tag): Unit = {
    System.err.println(summary(tag.name)(messages))
  }
}
