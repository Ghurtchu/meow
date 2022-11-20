package experiments

object FunctorAndMonad {

  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  object Functor {
    def apply[F[_]](implicit functor: Functor[F]): Functor[F] = functor // summoner
  }

  trait Monad[F[_]] extends Functor[F] {
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  }

  object Monad {
    def apply[F[_]](implicit monad: Monad[F]): Monad[F] = monad // summoner
  }

  object domain {
    final case class User(id: String, name: String)
    final case class UserProfile(user: User, friends: List[User], pictureCount: Int)
  }

  import domain._

  trait UserService[F[_]] {
    def fetchUserById(id: String): F[User]
  }

  trait UserProfileService[F[_]] {
    def fetchUserProfileByUser(user: User): F[UserProfile]
  }

  trait UserInvitationService[F[_]] {
    def send(userProfile: UserProfile): F[Unit]
  }

  class UserServiceLive extends UserService[Option] {
    override def fetchUserById(id: String): Option[User] = Some(User("aaa-bbb-ddd", "Nika"))
  }

  class UserProfileServiceLive extends UserProfileService[Option] {
    override def fetchUserProfileByUser(user: User): Option[UserProfile] = Some(UserProfile(user, User("bbb", "Tornike") :: User("ccc", "Guram") :: Nil, 5))
  }

  class UserInvitationServiceLive extends UserInvitationService[Option] {
    override def send(userProfile: UserProfile): Option[Unit] = Some(())
  }

  // how to compose them?
  // 1) get user by id
  // 2) use user to get user profile
  // 3) use user profile to send invitation

  val program = for {
    user        <- new UserServiceLive().fetchUserById("aaa-bbb-ddd")
    userProfile <- new UserProfileServiceLive().fetchUserProfileByUser(user)
    _           <- new UserInvitationServiceLive().send(userProfile)
  } yield ()

}
