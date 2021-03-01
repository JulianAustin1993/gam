package com.github.julianaustin1993.gam.examples

import breeze.linalg.csvread
import breeze.stats.mean
import com.github.julianaustin1993.gam.family.Binomial
import com.github.julianaustin1993.gam.glm.Glm
import com.github.julianaustin1993.gam.glm.Glm.predict

object logisticRegression {
  def main(args: Array[String]): Unit = {

    val url = "https://archive.ics.uci.edu/ml/machine-learning-databases/00267/data_banknote_authentication.txt"
    val fileName = "./data/banknote.csv"

    // download the file to disk if it hasn't been already
    val file = new java.io.File(fileName)
    if (!file.exists) {
      val s = new java.io.PrintWriter(file)
      val data = scala.io.Source.fromURL(url).getLines()
      data.foreach(l => s.write(l + "\n"))
      s.close()
    }

    // read the file from disk
    val mat = csvread(new java.io.File(fileName))
    println("Dim: " + mat.rows + " " + mat.cols)
    val y = mat(::, 4) // response is the final column
    val X = mat(::, 0 to 3)
    val mod = Glm(y, X, Binomial(), addIntercept = true)
    val predictions = predict(X, mod, response = true, addIntercept = true)
    println(mean((predictions - y) *:* (predictions - y)))
  } // main


}
