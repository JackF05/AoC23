import Data.List (sortBy)
import Data.List
import Data.List (maximumBy)
import Data.Function (on)
import Data.Char
import Data.Ord (comparing)

countLetters :: String -> String -> [(Char, Int)]
countLetters [] _ = []
countLetters (s:ss) str = (s, length $ (filter (== s) str)) : countLetters ss str

replace :: (Char, Int) -> (Int, Int)
replace ('T', x) = (10, x)
replace ('J', x) = (11, x)
replace ('Q', x) = (12, x)
replace ('K', x) = (13, x)
replace ('A', x) = (14, x)
replace (c, x) = (digitToInt c, x)

replace' :: (Char, Int) -> (Int, Int)
replace' ('T', x) = (10, x)
replace' ('J', x) = (1, x)
replace' ('Q', x) = (12, x)
replace' ('K', x) = (13, x)
replace' ('A', x) = (14, x)
replace' (c, x) = (digitToInt c, x)

repl :: String -> String
repl [] = []
repl (s:ss)
    | s == 'T' = ':' : repl ss
    | s == 'J' = ';' : repl ss
    | s == 'Q' = '<' : repl ss
    | s == 'K' = '>' : repl ss
    | s == 'A' = '?' : repl ss
    | otherwise   = s : repl ss

repl2 :: String -> String
repl2 [] = []
repl2 (s:ss)
    | s == 'T' = ':' : repl ss
    | s == 'J' = ',' : repl ss
    | s == 'Q' = '<' : repl ss
    | s == 'K' = '>' : repl ss
    | s == 'A' = '?' : repl ss
    | otherwise   = s : repl ss


rep :: String -> Char -> String
rep [] _ = []
rep (c:cs) l 
    | c == 'J'  = l : rep cs l
    | otherwise = c : rep cs l

rep2 :: String -> String -> [String]
rep2 _ [] = []
rep2 s (l:ls) = rep s l : rep2 s ls

concatTuple :: [(Char, Int)] -> String
concatTuple [] = []
concatTuple ((c, 1):ts) = c : concatTuple ts
concatTuple ((c, x):ts) = c : concatTuple ((c, x-1) : ts)


difference :: [(Char, Int)] -> [(Char, Int)] -> [(Char, Int)] -> [(Char, Int)]
difference [] _ _ = []
difference ((x, n):xs) [] o = (x, n) : difference xs o o -- not found element at all
difference ((x, n):xs) ((y, m):ys) o
    | x == y && n == m = difference xs o o -- found match
    | x == y && n /= m = (x, n - m) : difference xs o o -- one has more than the other
    | otherwise = difference ((x, n):xs) ys o -- not found yet so keep looking


rep3 :: [(Char, Int)] -> String -> String
rep3 [] s = s
rep3 ((x, 0):xs) s = rep3 xs s
rep3 w@((x, n):xs) s@(c:cs)
    | c == 'J'  = x : rep3 ((x, (n - 1)):xs) cs
    | otherwise = c : rep3 w cs

jCount :: [(Int, Int)] -> Int
jCount [] = 0
jCount ((11, n):js) = n
jCount ((_, _):js) = jCount js

main = do
    let smallCardInput = ["32T3K", "T55J5", "KK677", "KTJJT", "QQQJA"]
    let smallBets = [765, 684, 28, 220, 483]
    
    let cardInput = ["3J4KT", "Q3K42", "29JQK", "AAKA9", "555J5", "64JQ2", "A29J2", "33399", "AJ7A8", "3AA83", "9J979", "78478", "96644", "JKK8T", "33924", "56J42", "28J66", "44494", "93939", "62224", "4T3TT", "69J99", "7ATJ7", "8325T", "T3444", "99J9A", "5J474", "Q89K2", "7KJJK", "53A33", "TQ5QA", "TQJK5", "JQ4QQ", "TQ683", "QKA6A", "2T456", "TKT7K", "Q96AJ", "TTTT8", "9999A", "6KA94", "Q4A44", "3QKKQ", "Q33Q3", "83333", "3Q4AJ", "84T8A", "9T444", "K3A83", "8TTJ6", "Q5442", "K64K6", "969K6", "KQ333", "Q8277", "A5Q9A", "7K22K", "T459Q", "7KT35", "AJ6J6", "888KK", "5AA55", "667AK", "44447", "Q2K77", "K54AT", "787QA", "K87QK", "72AAQ", "92279", "993Q7", "98488", "55584", "KKK98", "6T666", "4A27T", "2789J", "7T77T", "T49J2", "TT97T", "4666K", "QQ38Q", "TT44J", "77A66", "5867T", "2TTT2", "Q443Q", "J5995", "2A3K7", "JKK23", "J38T4", "255Q2", "98898", "7Q74Q", "54544", "99349", "TATJA", "335J3", "25222", "TJ7T8", "J999J", "AAA35", "39964", "A7Q96", "4TT48", "K7567", "5TT78", "48568", "5J885", "54582", "JQATQ", "6646T", "9A855", "3333A", "45494", "88833", "8838T", "K69T4", "8A84A", "3JQ84", "KJ446", "82A2T", "Q385K", "K44KK", "8QKJ4", "37J33", "8T559", "JA55A", "A682T", "K3782", "77737", "TTT99", "66Q9Q", "Q2277", "3T3TT", "8365J", "JJ555", "83338", "A2Q99", "Q8J84", "A3467", "J6525", "J5838", "T3J29", "45K29", "77752", "28674", "25575", "24QQA", "9T352", "354Q9", "TJK24", "9J2AQ", "AKAAA", "999TT", "77277", "2626J", "A8Q3J", "88676", "QQ7TJ", "22T9T", "47474", "44548", "72JJ2", "365Q6", "22424", "K62KK", "7373Q", "QAJ26", "5AA99", "82228", "J3KK5", "A6T2J", "KKKK4", "62TTT", "33QJQ", "98999", "22JK4", "86886", "9A4JJ", "77974", "66366", "KKTJA", "J6445", "7T9QT", "J9A9A", "QAAAQ", "69966", "83348", "J45JQ", "TQK4A", "TTQTT", "566K4", "77747", "44K44", "JTTTT", "5Q5Q5", "4T2A5", "JK2K7", "52KT4", "35496", "4466J", "J7477", "77979", "JKQ8K", "9J3KJ", "3333J", "KK792", "4646A", "75745", "JAT5A", "2223J", "57577", "T9T44", "J48Q4", "8A888", "6Q6Q5", "3J3AJ", "JJJ8J", "5T637", "TTJ7Q", "88J3J", "K2238", "QAQJQ", "J272Q", "7QKKJ", "7399J", "7JTQJ", "4KAT9", "KK88J", "T267Q", "T2T7J", "5JK85", "8552K", "K6TQT", "93KQ4", "7J777", "KTKKJ", "55568", "J76QK", "T6683", "3929K", "22727", "J667A", "2292A", "K9324", "J7555", "A3T33", "A3AA3", "K4TJ4", "25379", "JJA62", "9K8AQ", "55455", "642T9", "J95Q2", "3Q925", "97TQ6", "K6KAK", "J6TKJ", "5945Q", "KT9A3", "2588K", "AJ933", "48644", "63336", "47593", "43TT3", "8858T", "4Q486", "AJ9T7", "33525", "TK5TT", "79777", "8TQQ3", "2TQK6", "4KK37", "66776", "AAJA6", "662K6", "55999", "555AT", "9479J", "77957", "6A333", "744K4", "95999", "6QQQQ", "7K57K", "67K6K", "29T2J", "2T5KJ", "58K94", "J2TK9", "99J22", "9989K", "2K9T4", "72872", "K37TQ", "634T7", "444JT", "QQ66A", "KJ755", "K5588", "33536", "Q6J58", "544A4", "7KQ7K", "4J2J3", "Q72J7", "4K4AK", "692A4", "22223", "22Q2J", "K6KK4", "25A7Q", "777Q7", "99898", "5A857", "54K94", "5J5Q5", "88288", "29922", "KKKT8", "9J5AA", "TTT6J", "8K88J", "AAAAJ", "62262", "2A626", "22J2T", "K2788", "Q838Q", "6T8TT", "TK4KK", "AKQAA", "3K78J", "TKK33", "27JAJ", "JK3TT", "4K3TQ", "36494", "58328", "5676J", "K4279", "66656", "4634A", "48A4A", "TTT6T", "K4678", "226QQ", "99J5Q", "9KJ99", "TT5T8", "965K7", "38334", "6J66Q", "9AA63", "JA742", "J5KKJ", "53A2T", "43J55", "2A2A9", "A8899", "8J333", "AQ282", "JKQ7Q", "95539", "93TT9", "7Q87J", "44TQ6", "63333", "AA8QQ", "AJAJJ", "66446", "T8T9T", "4A7TQ", "35J74", "J3399", "373J8", "55368", "J9KTA", "52555", "77272", "24T42", "KK4KJ", "KQJ95", "8J88J", "TQ384", "J9QQT", "44228", "KK52K", "4742Q", "346T3", "33339", "4AJ6K", "93Q27", "A2222", "Q2229", "A5355", "34333", "K9K94", "A59K8", "6T4AA", "JJJ99", "KAK3K", "JA2A2", "4KA9Q", "25666", "8T4T7", "7KK26", "666Q4", "4A99A", "7TQJ8", "8482J", "JJ848", "33K33", "234T4", "AK57T", "AJQ49", "A864A", "33T3T", "9AJK6", "64645", "6Q347", "T65K3", "5J6J5", "4545Q", "Q5888", "9QQT9", "6QAJ4", "666AK", "4827K", "7847A", "T66KT", "49A4T", "22272", "4TT68", "Q7K3A", "226Q6", "JK7K7", "QQ727", "646JQ", "5T729", "3K383", "989T9", "3J336", "563TA", "KK2AA", "J6K3T", "K999K", "A288A", "3T5J8", "J3J68", "7KT77", "336QQ", "3478K", "2K22K", "J4Q33", "876J2", "T5552", "TT2TT", "A2A25", "Q6J6Q", "AA3TA", "Q2T3Q", "9JK22", "QQQ8Q", "9J3Q5", "J6363", "77283", "9K962", "68787", "7A72A", "79KA8", "95965", "35596", "49997", "833J8", "444TT", "4646Q", "3QAJQ", "8T548", "348Q7", "55K5K", "A8A8A", "A88J8", "33AKQ", "48886", "54T45", "QJTTQ", "AAQA3", "4Q565", "Q56JK", "5QQQ5", "4AKJ8", "4AQQ5", "JTTJ5", "QQ5KQ", "2T537", "3QQQQ", "43K77", "T93JJ", "85525", "44448", "55474", "9965T", "33QQQ", "6A3Q5", "944J4", "99K92", "75557", "A4444", "T2TTK", "6A882", "QQ7QQ", "AK3K3", "5J5JJ", "K9KK6", "A3K33", "75J77", "288K2", "97JQJ", "8J357", "42Q5T", "75JA5", "9QQ9Q", "34322", "5KJK5", "666JA", "K99KK", "33T3J", "A3434", "QJQQJ", "5J22J", "3A377", "3T388", "436AA", "88889", "575A7", "K5KKJ", "5682J", "95AJ8", "8AA66", "QQQ77", "9989J", "888K8", "T4JJ9", "JK988", "58988", "J9929", "KAA7J", "TTTJJ", "7T7J7", "6T372", "276A3", "34AQK", "T9Q32", "4A4J5", "333KK", "QQJQQ", "KKK3K", "Q8Q88", "5K2AA", "K6666", "97335", "KAK4K", "42442", "T2A92", "7KKKK", "2KK2K", "24992", "24QK4", "664J3", "T55QQ", "343Q3", "T8A55", "58JQ5", "3843T", "3AQ96", "T69KT", "545K8", "28Q66", "44777", "T22Q8", "22J22", "96599", "KKQ7Q", "64Q7K", "Q3JQ8", "33373", "55QA5", "7J266", "9982A", "9TK6Q", "KK2JJ", "9Q9JQ", "AAAQA", "J5K4Q", "JK69K", "499A9", "2Q456", "4KAK7", "QQQKK", "AAAJ5", "67K23", "337J7", "T55T5", "AATT5", "T9TTT", "33387", "T57AQ", "652J2", "27788", "77JTT", "8826Q", "J7JKJ", "8J228", "23932", "3TJJ3", "7557K", "95JJ4", "6T6TA", "QQQTQ", "88228", "JT3T2", "66744", "33A3A", "77AA7", "337Q5", "88887", "8QQA7", "56622", "AJQKK", "TAJ7T", "66466", "78A8Q", "88A83", "72J5T", "2A534", "J33KT", "63223", "35552", "33JAA", "5A7A7", "J33J3", "TT93T", "JJJJJ", "T8TJQ", "7T9TJ", "944TJ", "43774", "592K9", "6J666", "JJ666", "9J668", "Q643K", "2Q2A2", "2936T", "A5A52", "3A4KJ", "8KAQ5", "38889", "TQ4QK", "AQK32", "79KK9", "J44J4", "568A8", "JA8J2", "A2386", "29374", "2Q2QJ", "5KKK5", "7QQ77", "723J7", "JTKK3", "85947", "666Q9", "9A847", "44JQA", "59Q8Q", "7QT64", "73J7T", "ATAAA", "7JJAQ", "KKKAK", "T659T", "Q9J99", "6J878", "JA5JA", "77769", "48J5J", "88787", "68K79", "A44T4", "T9JJK", "8A639", "K856Q", "26336", "22292", "2J268", "2A277", "J8288", "TQQQT", "22AAA", "52749", "AJA7A", "9Q999", "K2727", "J9A92", "2336J", "88T68", "55TTT", "7A5Q6", "QJT34", "77655", "79K5Q", "AQJQ8", "7T22T", "J554K", "A5AAT", "Q5K74", "QQJ3Q", "TKQ66", "377Q7", "3A4A3", "9J999", "93522", "5JQK7", "QQT66", "93K35", "7QQJJ", "AAT4J", "JT8TT", "528KQ", "AAJAJ", "QQ9Q7", "TTJ33", "39KJ7", "77J37", "5K555", "7TA4A", "5847J", "K527T", "TQ6Q9", "J6QJT", "TTJ88", "8A8A7", "A32T8", "769T3", "QK653", "666J3", "55Q55", "49286", "J4334", "97JA8", "25J79", "42JA5", "T3TJT", "97TKA", "QQ2QT", "86666", "5Q3JK", "3T333", "36693", "QKAAK", "778J8", "TQJ65", "2J228", "9869J", "A6QQ9", "746A6", "Q5678", "J2J83", "KKAK8", "J8TTJ", "T4333", "996Q7", "99696", "57J2J", "T269T", "89827", "QQ76Q", "4Q884", "5K43J", "99292", "Q53K3", "J8888", "39462", "JA43T", "AJ678", "676J7", "J9J89", "38AA8", "77J78", "9J944", "K2A22", "5QKJA", "J32J7", "5AA5A", "38647", "2AA32", "K9843", "JQ5A4", "6Q667", "5KA78", "62J5T", "4444J", "99QQ7", "4AAJA", "J9939", "93259", "85A55", "4JQ4Q", "66626", "34453", "AA8TT", "33555", "T3J63", "6J3T6", "7K7KK", "8QQ8Q", "79Q99", "Q5TQ8", "Q58QJ", "2224J", "67669", "83Q49", "J8834", "2Q2T6", "8TQQA", "TT9TA", "46436", "299T9", "72429", "A3695", "46468", "8K642", "ATQAQ", "QTJQQ", "9QQQQ", "4Q445", "22J26", "TATAT", "TTT44", "AK45Q", "545Q2", "5KJJ5", "63636", "K989K", "4644J", "6TJA6", "38KK8", "55J95", "Q3332", "Q5Q2J", "J7547", "A8753", "24525", "T5J5T", "AA565", "J3433", "K288T", "777T7", "TTJ22", "8TJ72", "8JA8A", "A55A3", "Q6247", "AAK2A", "A6886", "T6K8T", "66766", "KQQQQ", "JJ229", "J9AA3", "79J79", "695JK", "Q6Q6Q", "JA3QA", "27424", "65555", "99AAA", "KKKKQ", "QJ9QQ", "J2AAA", "29772", "48488", "5A7AA", "57773", "888J7", "5JJ45", "A3658", "42444", "77757", "Q8463", "99529", "KQK99", "86222", "66J86", "88666", "48838", "8QAJ5", "K555T", "KTKTK", "93693", "6AJJA", "9JT92", "KJJKK", "4AAQQ", "63553", "77878", "AJK8A", "77847", "64389", "KJ46K", "QQQQ2", "9TQQQ", "5JJ4J", "52JAQ", "22AJJ", "K3882", "65J65", "5A556", "K4445", "7J77J", "82922", "89K88", "J5666", "99T99", "46565", "63K6K", "Q8T9K", "88Q99", "QQ68Q", "KKKKJ", "QQQAA", "54T68", "T7382", "8J464", "6Q443", "4TJJQ", "4JJ4A", "4J49A", "TJA4T", "9Q99Q", "TJJ7T", "775AJ", "T82A8", "2Q55Q", "K3KJA", "666KK", "TA665", "68AKK", "95994", "34733", "KKK5K", "7KT7T", "K44J8", "Q7A9A", "JT3A3", "TJ938", "67677", "74T93", "Q6366", "78772", "KK22J", "55599", "9TAAA", "KQKKQ", "8KK87", "38J88", "57AQ9", "8793K", "377A2", "8487J", "J4JA2", "4T444", "9Q228", "68468", "8J8TK", "2K222", "9999K", "77T32", "8T88Q", "QQ5QQ", "J974T", "58575", "65JJJ", "4A92A", "3888K", "TJ5JA", "K5K65", "34433", "AA9AA", "A3323", "62AK3", "5542K", "76K27", "7T33T"]
    let bets = [513, 147, 187, 821, 571, 388, 877, 59, 312, 474, 218, 432, 502, 647, 127, 864, 208, 469, 167, 191, 447, 217, 99, 792, 545, 968, 724, 739, 565, 716, 573, 109, 436, 219, 711, 365, 292, 936, 22, 306, 376, 928, 29, 185, 318, 668, 696, 108, 245, 421, 482, 785, 685, 779, 606, 605, 631, 786, 856, 444, 297, 723, 838, 834, 238, 977, 755, 54, 278, 356, 310, 676, 835, 9, 603, 772, 954, 212, 102, 425, 900, 885, 89, 740, 517, 349, 338, 980, 350, 214, 481, 228, 922, 915, 976, 73, 382, 107, 950, 554, 960, 475, 301, 288, 851, 514, 398, 41, 848, 787, 309, 746, 235, 79, 560, 100, 781, 1, 234, 587, 173, 472, 7, 97, 570, 634, 898, 984, 616, 503, 777, 506, 653, 784, 859, 203, 855, 296, 884, 830, 158, 329, 196, 454, 72, 797, 60, 197, 42, 283, 46, 717, 580, 690, 811, 651, 96, 253, 595, 624, 239, 170, 180, 626, 667, 20, 277, 644, 806, 768, 895, 822, 540, 523, 155, 295, 812, 719, 518, 262, 563, 515, 192, 488, 468, 734, 664, 143, 531, 133, 679, 557, 677, 504, 135, 8, 207, 63, 901, 586, 674, 216, 162, 25, 714, 524, 271, 420, 182, 151, 413, 439, 569, 423, 769, 48, 308, 894, 910, 584, 845, 854, 736, 607, 286, 426, 298, 67, 989, 236, 126, 50, 839, 640, 336, 371, 867, 886, 707, 860, 30, 499, 378, 604, 429, 443, 123, 14, 478, 58, 470, 291, 729, 98, 341, 94, 279, 122, 198, 944, 463, 969, 742, 817, 450, 27, 509, 275, 92, 327, 862, 391, 56, 12, 215, 305, 243, 487, 507, 799, 803, 611, 497, 302, 691, 101, 863, 904, 818, 282, 810, 548, 355, 244, 671, 730, 138, 222, 912, 939, 783, 715, 709, 876, 567, 722, 501, 720, 117, 88, 103, 809, 705, 385, 623, 998, 702, 399, 335, 267, 280, 635, 204, 183, 141, 249, 251, 130, 659, 113, 654, 648, 935, 428, 572, 28, 320, 363, 324, 965, 422, 551, 166, 16, 313, 883, 104, 220, 979, 770, 948, 795, 596, 807, 268, 255, 511, 240, 699, 159, 921, 908, 790, 695, 78, 756, 525, 642, 2, 10, 972, 5, 990, 200, 461, 442, 789, 247, 849, 128, 281, 776, 601, 520, 311, 157, 112, 466, 537, 390, 655, 917, 788, 869, 796, 242, 713, 449, 638, 430, 77, 250, 210, 248, 542, 693, 550, 933, 516, 193, 744, 873, 703, 435, 688, 55, 791, 731, 406, 87, 160, 163, 617, 307, 1000, 315, 368, 66, 574, 500, 178, 660, 195, 246, 348, 270, 753, 725, 146, 759, 3, 85, 828, 946, 585, 966, 124, 878, 747, 150, 985, 408, 396, 75, 986, 612, 592, 962, 844, 637, 650, 353, 903, 417, 687, 816, 575, 752, 957, 625, 964, 538, 871, 24, 90, 344, 139, 258, 602, 924, 549, 342, 889, 893, 993, 264, 451, 213, 136, 303, 229, 942, 533, 636, 343, 322, 987, 492, 866, 34, 448, 347, 762, 657, 370, 325, 961, 940, 369, 483, 384, 773, 801, 263, 952, 751, 875, 618, 564, 345, 71, 820, 333, 741, 153, 26, 465, 874, 953, 257, 409, 23, 832, 847, 610, 256, 361, 373, 556, 467, 858, 74, 358, 186, 608, 620, 366, 566, 970, 47, 452, 206, 145, 645, 902, 121, 116, 154, 224, 857, 775, 633, 476, 205, 184, 18, 590, 831, 694, 598, 528, 505, 958, 782, 708, 678, 491, 780, 314, 735, 938, 673, 460, 332, 622, 252, 897, 393, 911, 231, 536, 559, 930, 774, 615, 484, 6, 819, 480, 843, 757, 473, 152, 290, 852, 749, 261, 861, 802, 86, 826, 583, 899, 681, 555, 718, 805, 745, 907, 532, 364, 273, 629, 870, 276, 738, 49, 241, 658, 700, 357, 351, 32, 937, 132, 490, 689, 665, 137, 149, 841, 981, 57, 853, 913, 339, 680, 287, 582, 433, 733, 743, 69, 971, 882, 129, 967, 415, 414, 704, 813, 323, 931, 156, 920, 266, 119, 561, 553, 37, 374, 562, 434, 15, 697, 19, 402, 441, 669, 945, 670, 652, 534, 161, 202, 232, 403, 649, 974, 367, 880, 701, 427, 597, 496, 823, 641, 416, 546, 169, 165, 609, 825, 956, 387, 106, 379, 527, 360, 44, 134, 778, 947, 223, 890, 93, 269, 362, 62, 793, 800, 579, 179, 951, 923, 464, 189, 672, 949, 914, 400, 544, 471, 254, 43, 576, 992, 639, 814, 386, 114, 211, 999, 453, 168, 526, 289, 682, 383, 692, 510, 80, 340, 896, 529, 837, 991, 140, 35, 588, 411, 21, 118, 621, 761, 684, 354, 836, 172, 84, 326, 446, 494, 177, 943, 995, 175, 916, 51, 683, 926, 906, 95, 225, 346, 530, 765, 627, 405, 988, 352, 996, 321, 199, 70, 438, 767, 401, 698, 458, 237, 131, 594, 754, 824, 418, 498, 840, 375, 221, 304, 918, 377, 827, 881, 656, 927, 982, 293, 850, 285, 983, 111, 661, 808, 599, 727, 65, 865, 489, 485, 558, 479, 404, 675, 372, 737, 978, 909, 833, 334, 233, 316, 395, 144, 174, 300, 380, 419, 227, 728, 115, 260, 389, 265, 959, 613, 666, 359, 91, 272, 763, 68, 381, 593, 905, 171, 299, 82, 994, 600, 105, 552, 81, 394, 40, 83, 11, 522, 804, 284, 632, 493, 294, 630, 319, 331, 424, 794, 190, 328, 226, 766, 431, 887, 259, 643, 397, 410, 125, 38, 181, 495, 748, 392, 535, 52, 486, 13, 581, 437, 577, 39, 462, 76, 892, 925, 412, 710, 209, 477, 750, 459, 17, 317, 829, 36, 726, 33, 591, 543, 706, 764, 619, 142, 457, 164, 521, 440, 512, 712, 568, 589, 798, 879, 188, 578, 868, 963, 407, 955, 120, 975, 662, 760, 31, 721, 732, 973, 815, 455, 888, 541, 53, 330, 663, 201, 686, 64, 547, 919, 176, 194, 846, 614, 771, 45, 456, 539, 872, 274, 758, 337, 934, 891, 628, 148, 646, 230, 110, 445, 61, 929, 508, 997, 4, 519, 932, 842, 941]
    
    -- Part 2
    let options = map (\c -> rep2 c "AKQT98765432") cardInput
    let counts = map (\o -> map (\c -> countLetters "AKQT98765432" c) o) options
    let counts' = map (\o -> countLetters o o) cardInput
    let filteredOptions = map (\o -> map (\c -> filter (\t -> snd t /= 0) c) o) counts
    let filtered' = map (\o -> filter (\t -> snd t /= 0) o) counts'
    --print(filtered')
    let orderedTuples = map (\o -> map (\c -> sortBy (flip compare `on` snd) c) o) filteredOptions
    let orderedOptions = map (\o -> maximumBy (comparing $ maximum . map snd) (reverse o)) orderedTuples
    let cards = map concatTuple orderedOptions

    let is' = [0..length orderedOptions - 1]
    let diff1 = map (\i -> difference (orderedOptions !! i) (filtered' !! i) (filtered' !! i)) is'
    let diff2 = map (\i -> difference (filtered' !! i) (orderedOptions !! i) (orderedOptions !! i)) is'
    let abc = map (\d -> filter (\t -> snd t > 0) d) diff2

    let is'' = [0..length cardInput - 1]
    let newCards = map (\i -> rep3 (diff1 !! i) (cardInput !! i)) is''

    --print(head orderedOptions)
    --print(head filtered')
    --print(diff1)
    --print(newCards)
    --print(head diff2)
    --print(abc)
    --let concatCards = 
    -- compare each inner most list against each other to find the highest first tuple second 
    -- element. then take the first one and that is the highest hand so take first element of
    -- each tuple in this list, concatenate and that is new cardInput

    --print(orderedOptions)

    let c'' = cardInput
    let exc = map (\e -> "AAAAA") (filter (== "JJJJJ") c'')
    let c' = c'' ++ exc

    let b = bets

    let z = zip c' b
    
    let count = map (\c -> countLetters "AKQJT98765432" c) c'
    let count' = map (\c -> countLetters "AKQT98765432J" c) c'
    --print(count)

    let changed = map (\c -> map replace c) count
    let changed' = map (\c -> map replace' c) count'

    let sorted = map (\c -> sortBy (flip compare `on` snd) c) changed
    let sorted' = map (\c -> sortBy (flip compare `on` snd) c) changed'

    let filtered = map (\c -> filter (\t -> snd t /= 0) c) sorted
    let filtered' = map (\c -> filter (\t -> snd t /= 0) c) sorted'

    let zipped = zip filtered z
    let zipped' = zip filtered' z

    let fives = filter (\c -> snd ((fst c) !! 0) == 5) zipped
    let fives' = filter (\c -> (snd ((fst c) !! 0) + jCount (fst c)) == 5) zipped'
    let f1 = zipped \\ fives
    let f1' = zipped \\ fives'

    let fours = filter (\c -> snd ((fst c) !! 0) == 4) f1
    let fours' = filter (\c -> (snd ((fst c) !! 0) + jCount (fst c)) == 4) f1'
    let f2 = f1 \\ fours
    let f2' = f1' \\ fours'

    let fulls = filter (\c -> snd ((fst c) !! 0 ) == 3 && snd ((fst c) !! 1) == 2) f2
    let fulls' = filter (\c -> (snd ((fst c) !! 0) + jCount (fst c)) == 3 && snd ((fst c) !! 1) == 2) f2'
    let f3 = f2 \\ fulls
    let f3' = f2' \\ fulls'

    let threes = filter (\c -> snd ((fst c) !! 0) == 3) f3
    let threes' = filter (\c -> (snd ((fst c) !! 0) + jCount (fst c)) == 3) f3'
    let f4 = f3 \\ threes
    let f4' = f3' \\ threes'

    let twoPairs = filter (\c -> snd ((fst c) !! 0 ) == 2 && snd ((fst c) !! 1) == 2) f4
    let twoPairs' = filter (\c -> snd ((fst c) !! 0 ) + jCount (fst c) == 2 && snd ((fst c) !! 1) == 2) f4'
    let f5 = f4 \\ twoPairs
    let f5' = f4' \\ twoPairs'

    let pairs = filter (\c -> snd ((fst c) !! 0) == 2) f5
    let pairs' = filter (\c -> (snd ((fst c) !! 0) + jCount (fst c)) == 2) f5'
    let highs = f5 \\ pairs
    let highs' = f5' \\ pairs'

    let allSets = [fives, fours, fulls, threes, twoPairs, pairs, highs]
    let allSets' = [fives', fours', fulls', threes', twoPairs', pairs', highs']

    let m = map (\s -> map (\t -> (fst t, (repl (fst $ snd t), snd $ snd t))) s) allSets
    let m' = map (\s -> map (\t -> (fst t, (repl2 (fst $ snd t), snd $ snd t))) s) allSets'
    print(m')
    let s = map (\s -> sortBy (flip compare `on` (fst . snd)) s) m
    let s' = map (\s -> sortBy (flip compare `on` (fst . snd)) s) m'

    let conc = concat s
    let conc' = concat s'

    let is = reverse [1..length conc]
    let is' = reverse [1..length conc']

    let vs = map (\c -> snd $ snd c) conc
    let vs' = map (\c -> snd $ snd c) conc'

    let vals = map (\i -> i * vs !! ((length vs) - i)) is
    let vals' = map (\i -> i * vs' !! ((length vs') - i)) is'
    print(sum vals)
    print(sum vals')