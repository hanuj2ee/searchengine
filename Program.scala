package test

import scala.io.StdIn.readLine
import java.io.File
import scala.util.Try
import scala.Left
import scala.io.Source
import scala.Right
import scala.collection.mutable.Map

/**
 * Program used to index the file and search th words in file
 */
object Program {

  case class Index(files: Map[String, Map[String, Int]]);
  sealed trait ReadFileError
  case object MissingPathArg extends ReadFileError
  case class NotDirectory(error: String) extends ReadFileError
  case class FileNotFound(t: Throwable) extends ReadFileError
  /**
   * Read files
   */
  def readFile(args: Array[String]): Either[ReadFileError, File] = {
    for {
      path <- args.headOption.toRight(MissingPathArg)
      file <- Try(new java.io.File(path)).fold(
        throwable => Left(FileNotFound(throwable)),
        file =>
          if (file.isDirectory) Right(file)
          else Left(NotDirectory(s"Path [$path] is not a directory")))
    } yield file
  }
  /**
   * Create index of file
   */
  def index(file: File): Index = {
    //List all files
    val files: Array[File] = (file)
      .listFiles
      .filter(_.isFile());
    val map: Map[String, Map[String, Int]] = Map();
    if (null != files && !files.isEmpty) {
      
      for (datafile <- files) {
        println("Processing of " + datafile + " started.");

        //Create the map of words and count. Count is not included in ranking alogirthem
        val dictionary: Map[String, Int] = Map();

        for (line <- scala.io.Source.fromFile(datafile).getLines()) {
          //Split the line and get words
          val parts: Array[String] = line.split("\\W+");
          for (word <- parts) {
            var count: Int = 0;
            if (dictionary.contains(word.trim())) {
              count = count + 1;
            }
            dictionary += (word.trim() -> count);
          }
        }
        map += (datafile.getName -> dictionary);
        println("Processing of " + datafile + " completed.");
      }
    }
    val index = Index(map);
    return index;
  }
  /**
   * Iterate of the file and
   */
  def iterate(indexedFiles: Index): Unit = {
    print(s"search> ")

    val searchString = readLine()
    //Return if string is ':quit'
    if (searchString.equals(":quit"))
      return ;
    else {
      //If string is not null or empty then process
      if (null != searchString && !searchString.trim().isEmpty()) {
        val parts: Array[String] = searchString.split(" "); //Split the string and get words
        if (null != parts && parts.length > 0) {
          var wordCount = parts.length; //Number of words
          //Check each word found in file. If found then increment the count
          for ((k, v) <- indexedFiles.files) {
            var count: Int = 0;
            //Count number of words found in a file
            for (word <- parts) {
              if (v.contains(word)) {
                count = count + 1;
              }
            }
            var rank = (count * 100) / wordCount //Calculate percentage by formula ( total number of words found in file * 100 ) /total number of words in search string
            print(k + ":" + rank + " % ")
          }
        }
        println();
      }
      iterate(indexedFiles)
    }
  }
}