namespace ConsoleApp3

module Domain =
    type UserId = UserId of int
    type UserName = string
    type EmailAddress = EmailAddress of string

    type Profile =
        { UserId: UserId
          Name: UserName
          EmailAddress: EmailAddress }

    type EmailMessage = { To: EmailAddress; Body: string }
