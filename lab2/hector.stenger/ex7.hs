someIban = "DE44500105175407324931"
otherIban = "GB82WEST12345698765432"

iban :: [Char] -> Bool
iban n = number `mod` 97 == 1
    where ibanNumber = arrayToInteger (map convertToInteger (moveFourToBack n))
          number = read ibanNumber :: Integer

arrayToInteger :: [Int] -> [Char]
arrayToInteger [x] = show x ++ []
arrayToInteger (x:xs) = show x ++ arrayToInteger xs

convertToInteger :: Char -> Int
convertToInteger s
    | i < 58 && i > 47 = i - 48
    | otherwise = i - 55 
    where i = fromEnum s

moveFourToBack :: [Char] -> [Char]
moveFourToBack n = drop 4 n ++ take 4 n

{- Example valid iban numbers copied from Piotr -}
validExamples = ["AL47212110090000000235698741",
  "AD1200012030200359100100",
  "AT611904300234573201",
  "AZ21NABZ00000000137010001944",
  "BH67BMAG00001299123456",
  "BE62510007547061",
  "BA391290079401028494",
  "BG80BNBG96611020345678",
  "HR1210010051863000160",
  "CY17002001280000001200527600",
  "CZ6508000000192000145399",
  "DK5000400440116243",
  "EE382200221020145685",
  "FO9754320388899944",
  "FI2112345600000785",
  "FR1420041010050500013M02606",
  "GE29NB0000000101904917",
  "DE89370400440532013000",
  "GI75NWBK000000007099453",
  "GR1601101250000000012300695",
  "GL5604449876543210",
  "HU42117730161111101800000000",
  "IS140159260076545510730339",
  "IE29AIBK93115212345678",
  "IL620108000000099999999",
  "IT40S0542811101000000123456",
  "JO94CBJO0010000000000131000302",
  "KW81CBKU0000000000001234560101",
  "LV80BANK0000435195001",
  "LB62099900000001001901229114",
  "LI21088100002324013AA",
  "LT121000011101001000",
  "LU280019400644750000",
  "MK07250120000058984",
  "MT84MALT011000012345MTLCAST001S",
  "MU17BOMM0101101030300200000MUR",
  "MD24AG000225100013104168",
  "MC9320052222100112233M44555",
  "ME25505000012345678951",
  "NL39RABO0300065264",
  "NO9386011117947",
  "PK36SCBL0000001123456702",
  "PL60102010260000042270201111",
  "PT50000201231234567890154",
  "QA58DOHB00001234567890ABCDEFG",
  "RO49AAAA1B31007593840000",
  "SM86U0322509800000000270100",
  "SA0380000000608010167519",
  "RS35260005601001611379",
  "SK3112000000198742637541",
  "SI56191000000123438",
  "ES8023100001180000012345",
  "SE3550000000054910000003",
  "CH9300762011623852957",
  "TN5910006035183598478831",
  "TR330006100519786457841326",
  "AE070331234567890123456"]

{- Example invalid iban numbers copied from Piotr -}
invalidExamples = [
  "PL601020102600042270201111",
  "PT51000201232234567890154",
  "QA68DOHB00001234567890ABCDSFG",
  "RO49AAAA1B31007593841100",
  "SM86U0322509855000000270100",
  "SA0330000000608010167519",
  "RS35260006501001611379",
  "SK3112002200198742637541",
  "SI56191000665123438",
  "ES80231000013000012345",
  "SE3550342220054910000003",
  "CH93007620233852957",
  "TN5915555183598478831",
  "TR3300061069786457841326",
  "AE070331234567896123456"]

validityCheck = all iban validExamples
invalidityCheck = all (\n -> not $ iban n) invalidExamples
