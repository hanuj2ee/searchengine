package test
/**
 * The main object of the application
 */
object SimpleSearch extends App {
  Program.readFile(args).fold(println, file => Program.iterate(Program.index(file)))
}