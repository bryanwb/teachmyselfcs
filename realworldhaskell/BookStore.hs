data BookInfo = Book Int String [String]
              deriving (Show)

data MagazineInfo = Magazine Int String [String]
                    deriving (Show)

myInfo = Book 9780135072455 "Algebra of Programming"
         ["Richard Bird", "Oege de Moor"]

type CardHolder = String
type CardNumber = String
type Address = [String]
type CustomerId = Int


data BillingInfo = CreditCard CardNumber CardHolder Address
                 | CashOnDelivery
                 | Invoice CustomerId
                 deriving Show

bookID (Book id title authors) = id
bookTitle (Book id title authors) = title
bookAuthors (Book id title authors) = authors


data Customer = Customer {
  customerId :: CustomerId
  , customerName :: String
  , customerAddress :: Address
  } deriving (Show)
