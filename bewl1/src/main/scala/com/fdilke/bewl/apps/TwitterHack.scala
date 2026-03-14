package com.fdilke.bewl.apps

import com.danielasfregola.twitter4s.{TwitterRestClient, TwitterStreamingClient}
import com.danielasfregola.twitter4s.entities.{AccessToken, ConsumerToken}

import scala.concurrent.Await
import scala.io.Source
import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.language.postfixOps

object TwitterHack {

  val TWITTER_CFG_PATH =
    "config/twitter.cfg"

  val Seq(
    consumerKey,
    consumerSecret,
    accessToken,
    accessSecret
  ) = {
    val lines =
      Source
        .fromFile(
          TWITTER_CFG_PATH
        )
        .getLines
        .toSeq

    Seq(
      "consumer-key",
      "consumer-secret",
      "access-token",
      "access-secret"
    ).map { key =>
      lines.find {
        _.startsWith(key + "=")
      } match {
        case None =>
          throw new IllegalArgumentException(
            "Can't find creds for " + key + " in config: " + TWITTER_CFG_PATH
          )
        case Some(line) =>
          line.split("=")(1)
      }
    }
  }

  println(
    s"found creds:" +
      consumerKey + "," +
      consumerSecret + "," +
      accessToken + "," + accessSecret
  )

  val client = new TwitterRestClient(
    ConsumerToken(consumerKey, consumerSecret),
    AccessToken(accessToken, accessSecret)
  )

  val felixId =
    Await
      .result(
        client.user("fdilke"),
        5 seconds
      )
      .data
      .id

  def tweetMe(message: String): Unit =
    //        println("user details: " + user)
    //        println("user name: " + user.data.name)
    client.createDirectMessageEvent(felixId, message).onComplete {
      case Success(dm) =>
//        println("Sent DM! " + dm)

      case Failure(_) =>
        println("DM send failed")
    }

//  tweetMe("And another!")
}
