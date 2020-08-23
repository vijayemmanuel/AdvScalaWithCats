package Extra

import scala.annotation.tailrec

object DirectoryEx {

  // ADT
  sealed trait DirectoryStructure
  case class Folder(name: String, folder : List[Folder], files: List[File]) extends DirectoryStructure
  case class File(name: String) extends DirectoryStructure

  val dir = Folder("F1", List(Folder("F2",Nil,List(File("T2"),File("T3")))), List(File("T1")))

  def getFile (folder : Folder): List[File] = {
    val files = folder.files
    files ++ folder.folder.flatMap(x => getFile(x))

  }

  def getFileForLoop(folder : Folder): List[File] = {
    var files = folder.files
    for (i <- 0 until folder.folder.length) {
      files = getFileForLoop(folder.folder(i)) ::: files
    }
    files
  }


  def main(args: Array[String]): Unit = {
    println(getFile(dir))
    println(getFileForLoop(dir))

  }

}
