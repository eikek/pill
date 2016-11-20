package pill

import java.nio.file._
import java.nio.file.attribute.PosixFilePermission._
import java.util.EnumSet
import scala.collection.JavaConverters._
import cats.syntax.either._

object files {
  private val utf8 = scala.io.Codec.UTF8

  val root = Paths.get(FileSystems.getDefault.getSeparator)
  val home = Paths.get(System.getProperty("user.home"))

  def wd = Paths.get("").toAbsolutePath

  def mkdir(p: Path): Either[Exception, Unit] =
    Either.catchOnly[Exception] {
      Files.createDirectories(p)
    }

  private def newFilter(f: Path => Boolean) = new DirectoryStream.Filter[Path] {
    def accept(p: Path) = f(p)
  }

  implicit final class PathOps(val _p: Path) extends AnyVal {
    private def path = _p.normalize().toAbsolutePath
    def absolute = path

    def /(next: String): Path = path.resolve(next)
    def /(next: Int): Path = /("%06d".format(next))

    def exists: Boolean = Files.exists(path)
    def notExists: Boolean = !exists

    def isFile: Boolean = Files.isRegularFile(path)
    def fileExists: Boolean = isFile && exists

    def name: String = path.getFileName.toString
    def parent: Path = path.getParent
    def pathString: String = path.toString

    def >>:(data: String): Either[Exception, Unit] =
      Either.catchOnly[Exception] {
        Files.write(path, data.getBytes(utf8.charSet))
      }

    def contents: Either[Exception, String] =
      Either.catchOnly[Exception] {
        new String(Files.readAllBytes(path), utf8.charSet)
      }

    def rmR(): Either[Exception, Unit] = {
      def loop(f: Path): Unit = {
        if (Files.isDirectory(f)) {
          val c = Files.newDirectoryStream(f)
          try { c.asScala.foreach(_.rmR) } finally { c.close }
        } else {
          Files.delete(f)
        }
      }
      Either.catchOnly[Exception](loop(path))
        .flatMap(_ => Either.catchOnly[Exception](Files.deleteIfExists(path)))
    }

    def list(filter: Path => Boolean = _ => true): Either[Exception, Vector[Path]] =
      Either.catchOnly[Exception] {
        val files = Files.newDirectoryStream(path, newFilter(filter))
        try { files.asScala.toVector } finally { files.close }
      }

    def mod755(): Either[Exception, Unit] =
      Either.catchOnly[Exception] {
        Files.setPosixFilePermissions(path, EnumSet.of(
          OWNER_READ, OWNER_WRITE, OWNER_EXECUTE,
          GROUP_READ, GROUP_EXECUTE,
          OTHERS_READ, OTHERS_EXECUTE
        ))
      }
  }
}
