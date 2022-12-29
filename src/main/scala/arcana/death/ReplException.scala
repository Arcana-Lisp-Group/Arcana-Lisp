package arcana.world

final case class ReplException(
    private val message: String = "",
    private val cause: Throwable = None.orNull
) extends Exception(message, cause)
