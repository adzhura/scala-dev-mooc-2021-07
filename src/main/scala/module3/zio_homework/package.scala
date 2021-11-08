package module3

import zio.clock._
import zio.console.{Console, getStrLn, putStrLn}
import zio.duration.durationInt
import zio.random._
import zio.{ExitCode, Has, IO, UIO, ULayer, URIO, ZIO, ZLayer}

import java.io.IOException
import java.util.concurrent.TimeUnit
import scala.language.postfixOps

package object zio_homework {
  /**
   * 1.
   * Используя сервисы Random и Console, напишите консольную ZIO программу которая будет предлагать пользователю угадать число от 1 до 3
   * и печатать в когнсоль угадал или нет. Подумайте, на какие наиболее простые эффекты ее можно декомпозировать.
   */

  def random(lowerBound: Int, upperBound: Int): URIO[Random, Int] = nextIntBetween(lowerBound, upperBound + 1)

  def userInput(): ZIO[Console, IOException, String] = for {
    _ <- putStrLn("Enter a number:")
    input <- getStrLn
  } yield input

  def intInput(): URIO[Console, Int] = userInput().flatMap(str => ZIO.effect(str.toInt))
    .orElse(putStrLn("Wrong input. Try again.") *> intInput())

  def guessSecret(secret: Int): URIO[Console, Int] = for {
    input <- intInput()
    number <- ZIO.fromOption(if (secret == input) Some(secret) else None)
      .orElse(putStrLn("Wrong number, Try again.") *> guessSecret(secret))
  } yield number

  lazy val guessProgram: URIO[Random with Console, ExitCode] = for {
    secret <- random(1, 3)
    _ <- putStrLn("Try to guess the number from 1 to 3")
    number <- guessSecret(secret)
    _ <- putStrLn(s"Bingo! You guessed $number")
  } yield ExitCode.success

  /**
   * 2. реализовать функцию doWhile (общего назначения), которая будет выполнять эффект до тех пор, пока его значение в условии не даст true
   *
   */

  def doWhile[R, E, A](zio: ZIO[R, E, A])(condition: A => Boolean): ZIO[R, E, A] = for {
    a <- zio
    _ <- ZIO.effect(condition(a)).orDie
      .flatMap(flag => if (flag) ZIO.succeed(a) else doWhile(zio)(condition))
  } yield a

  /**
   * 3. Реализовать метод, который безопасно прочитает конфиг из файла, а в случае ошибки вернет дефолтный конфиг
   * и выведет его в консоль
   * Используйте эффект "load" из пакета config
   */

  def loadConfigOrDefault: URIO[Console, ExitCode] = {
    import config._
    for {
      cfg <- load <> ZIO.succeed(AppConfig("app", "host.com"))
      _ <- putStrLn(cfg.toString)
    } yield ExitCode.success
  }

  /**
   * 4. Следуйте инструкциям ниже для написания 2-х ZIO программ,
   * обратите внимание на сигнатуры эффектов, которые будут у вас получаться,
   * на изменение этих сигнатур
   */


  /**
   * 4.1 Создайте эффект, который будет возвращать случайеым образом выбранное число от 0 до 10 спустя 1 секунду
   * Используйте сервис zio Random
   */
  lazy val eff: URIO[Random with Clock, Int] = for {
    _ <- sleep(1.second)
    n <- random(0, 10)
  } yield n


  /**
   * 4.2 Создайте коллукцию из 10 выше описанных эффектов (eff)
   */
  lazy val effects: Seq[URIO[Random with Clock with Console, Int]] = Seq.fill(10)(eff)


  /**
   * 4.3 Напишите программу которая вычислит сумму элементов коллекци "effects",
   * напечатает ее в консоль и вернет результат, а также залогирует затраченное время на выполнение,
   * можно использовать ф-цию printEffectRunningTime, которую мы разработали на занятиях
   */

  lazy val app: URIO[Clock with Random with Console, Int] = for {
    sum <- zioConcurrency.printEffectRunningTime(effects.reduceLeft((z1, z2) => z1.zipWith(z2)(_ + _)))
    _ <- putStrLn(s"Sum = $sum")
  } yield sum


  /**
   * 4.4 Усовершенствуйте программу 4.3 так, чтобы минимизировать время ее выполнения
   */

  lazy val appSpeedUp: URIO[Clock with Random with Console, Int] = for {
    sum <- zioConcurrency.printEffectRunningTime(effects.reduceLeft((z1, z2) => z1.zipWithPar(z2)(_ + _)))
    _ <- putStrLn(s"Sum = $sum")
  } yield sum


  /**
   * 5. Оформите ф-цию printEffectRunningTime разработанную на занятиях в отдельный сервис, так чтобы ее
   * молжно было использовать аналогично zio.console.putStrLn например
   */
  type Benchmark = Has[Benchmark.Service]

  object Benchmark {
    trait Service {
      def printEffectRunningTime[R, E, A](zio: ZIO[R, E, A]): ZIO[Clock with Console with R, E, A]
    }

    val live: ULayer[Benchmark] = ZLayer.succeed(new Service {
      override def printEffectRunningTime[R, E, A](zio: ZIO[R, E, A]): ZIO[Clock with Console with R, E, A] =
        for {
          start <- currentTime(TimeUnit.SECONDS)
          z <- zio
          end <- currentTime(TimeUnit.SECONDS)
          _ <- putStrLn(s"Running benchmark: ${end - start}s")
        } yield z
    })

    def printEffectRunningTime[R, E, A](zio: ZIO[R, E, A]): ZIO[Benchmark with Clock with Console with R, E, A] =
      ZIO.accessM(_.get.printEffectRunningTime(zio))
  }

  /**
   * 6.
   * Воспользуйтесь написанным сервисом, чтобы созадть эффект, который будет логировать время выполнения прогаммы из пункта 4.3
   *
   *
   */

  lazy val appWithTimeLogg: ZIO[Benchmark with Clock with Console with Random, Nothing, Int] =
    for {
      sum <- Benchmark.printEffectRunningTime(effects.reduceLeft((z1, z2) => z1.zipWith(z2)(_ + _)))
      _ <- putStrLn(s"Sum = $sum")
    } yield sum

  /**
   *
   * Подготовьте его к запуску и затем запустите воспользовавшись ZioHomeWorkApp
   */
  lazy val runApp: ZIO[Clock with Random with Console, Nothing, Int] =
    appWithTimeLogg.provideSomeLayer[Clock with Random with Console](Benchmark.live)

}
