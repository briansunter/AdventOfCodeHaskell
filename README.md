# Advent of Code in Haskell
The [Advent of Code](http://adventofcode.com) is a collection of fun programming puzzles. Each problem has two parts. The second part is a slight variation on the first and is only revealed after the first is solved. I really like this format because it simulates the changing of requirements that happens in the real world. A good test of the quality and flexibility of our code will be how easily we can adapt our first solution to solve the requirements of the second.

## Goals

I want to improve my skills in the Haskell programming language, highlight some of the advantages of Haskell, and document my approach to solving problems. My goal is write answers that model the problem domain as clearly as possible, are composable and flexible enough to solve the second part with minimal changes, and have good performance. I don't want to write a Monad or Applicative tutorial, so I'll just gloss over these concepts in the later problems.

## [Day 1: Not Quite Lisp](http://adventofcode.com)
### Testing

Each of the puzzles provide examples of correct answers and their inputs. These serve as perfect test cases so lets start with the tests. I'm using a library called [HSpec](http://hspec.github.io/) which looks almost identical to Ruby's [Rspec](http://rspec.info/) library. Haskell's type inference allows us to write code as clean as Ruby, but with the advantages of strong typing. We know for sure that we're comparing the correct values before we run it and we get lots of editor support such as excellent autocompletion, hover over type hints, and jump to definition.

```
describe "Day7 Part 1" $ do
  it "should run the examples" $ do
    santaFloor "(())" `shouldBe` 0
    santaFloor "))(((((" `shouldBe` 3
    santaFloor "(((" `shouldBe` 3
```


### Part 1
Let's start by breaking down the problem. We're given a bunch of input and need to reduce it to a single number. A common functional pattern called `fold` does exactly that. If you didn't know that off the top of your head you could play around with the Haskell search engine [Hoogle](https://www.haskell.org/hoogle/?hoogle=%28a+-%3E+b+-%3E+b%29+-%3E+b+-%3E+%5Ba%5D+-%3E+b) and try to come up with the type signature you need `(a -> b -> a) -> a -> [b] -> a`

Folds have a concept of an "accumulator" which can be thought of as the "value so far"

In its type signature`foldl' :: (a -> b -> a) -> a -> [b] -> a`
* `(a -> b -> a)` is a function that takes an accumulator  `a` and the next input `b`, and returns an updated accumulator `a`.
* `a` is the starting value of the accumulator
* `[b]` is the list of inputs
* `a` is the final result of the accumulator after being updated by each input

There are a number of different folds with characteristics and there is a good discussion [here](https://wiki.haskell.org/Foldr_Foldl_Foldl'). We'll use `foldl'` because it has the best performance for large finite lists.

We want to read the input one by one and increase the current floor number when we encounter `(` and decrease it when we see  `)`. Lets write the function that takes the current floor, and the next paren from the input, and returns the new floor.

```
step :: Int -> Char -> Int
step fl x = if x == '(' then fl + 1 else fl - 1
```

Now that we've solved it for one floor, we just plug it into `fold` along with a starting floor. `fold` will then feed the list in one by one changing the current floor along the way.

```
santaFloor :: String -> Int
santaFloor s = foldl' step 0 s

```

If your editor has Haskell language support with a tool called ghc-mod, this code will type check, but will give you the warning:
```
Eta reduce
Found:
  santaFloor s = foldl' step 0 s
Why not:
  santaFloor = foldl' step 0
```

Our editor has found a simpler form for us. We can eliminate the input parameter due to a Haskell feature called partial function application. In most languages you need to pass all the parameters into a function before you can do anything with it. In Haskell, If you have a function that takes 3 arguments, and pass 1 in, it returns a function that takes 2 arguments, with the first already applied. In our answer, we can leave off the last input. Since we've only applied 3 out of 4 parameters, we now have a function expecting one more parameter. Our final solution is

```
santaFloor :: String -> Int
santaFloor = foldl' step 0
```

### Part 2
Part 2 is now revealed and asks us to find the position of the first character that causes him to enter the basement. We will also handle the case where he never enters the basement, even though we can assume he will enter given the problem format.

What we really want is a timeline of how his floor changes over time, and find the first point where his floor number is `-1` (since we know he only goes up or down one floor at a time).

We currently have `foldl'` which gives us the ending floor:
`(a -> b -> a) -> a -> [b] -> a`
and we now want almost exactly the same thing, but a list of all the floors as they change with each input:
`(a -> b -> a) -> a -> [b] -> [a]`

Thankfully, there is another function that does exactly that called `scanl`. We can reuse all the logic from Part 1 and just plug our `step` function into it to get a timeline of Santa's floors.
```scanl step 0```

Now, all we need to do is find the index of the first `-1` element (Which may not exist).

We can use the function `elemIndex :: a - > [a] -> Maybe a` to do this.

The type Haskell `Maybe` is a value that might not exist. It is either `Just a` or `Nothing`.
 In other languages a `null` is usually returned to signify that the element was not found. Unfortunately, it's easy to forget to continually check for `null` and your program will crash at runtime if you forget. `Maybe` forces us to account for the possibility that the element might not be in the list. We cannot use the value in a `Maybe` unless we check for `Nothing` and handle it.

 Now we put the pieces together and get our solution.
 ```
 santaFloorBasement :: String -> Maybe Int
 santaFloorBasement  = elemIndex (-1) scanl step 0
 ```

### Some Extra Testing
Another great testing library is [QuickCheck](https://hackage.haskell.org/package/QuickCheck).
Instead of relying upon ourselves to manually come up with test cases and doing our best to identify edge cases, we think about properties that must always hold for our function and have QuickCheck automatically generate tons of input and attempts to find the simplest counter example that disproves our property. Other languages also have "Fuzz Testing" but the extra information from the types and some of Haskell's language features make QuickCheck easier to use. A great introduction can be found [here](https://www.schoolofhaskell.com/user/pbv/an-introduction-to-quickcheck-testing).

Lets write our own property test. We'll generate two random length lists of `(` and `)`. Since we're generating the input, we can just subtract the number of `(` from the number of `)` and know what the correct answer should be ahead of time. We then shuffle the lists together to generate random test inputs, answers. QuickCheck will generate edge cases such as empty lists and very long lists.

```
testEndingFloorInput :: Gen (String,Int)
testEndingFloorInput = do
  closed  <- listOf $ elements ")"
  open <- listOf $ elements "("
  shuffled <- shuffle $ open ++ closed
  return (shuffled, length open - length closed)
```

Then we'll make sure every test QuickCheck generates arrives at the right answer.

```
  testCorrectEndingFloor = forAll testEndingFloorInput (\(ins, expected) -> santaFloor ins == expected )
```

Lets think of some properties for Part 2. If there are more `(` than `)` he might not enter the basement, depending on their order. However, if there are more `)` than `(` than we know for sure, that he must eventually enter the basement at some point.

Let generate random test input that has more `)` than `(`

```
testBasementInput  :: Gen String
testBasementInput = do
  closed  <- listOf1 $ elements ")"
  openSized <- choose(0, length closed - 1)
  open <- vectorOf openSized $ elements "("
  shuffle $ open ++ closed
  ```

  And we'll make sure that he goes into the basement at some point for each of these randomly generated inputs.
```
  testDoesEnterBasement = forAll testBasementInput $ isJust . santaFloorBasement
```

It's a little overkill for a simple problem, but it can help you find edge cases that you might not think of. [Check out some really good examples of QuickCheck on problems like  "The Mersenne conjecture of antiquity", "The Collatz conjecture", and "red-black tree deletion".](http://matt.might.net/articles/quick-quickcheck/)

## [Day 2: I Was Told There Would Be No Math](http://adventofcode.com/day/2)
Day 2 asks us to reduce some input into a single number. Sound familiar? We'll use the same pattern from Day 1.

We're given lines of dimensions in the form Length x Width x Height. Lets make a type alias for convenience and readability.

```
type Dimensions = (Integer, Integer, Integer)
```
Dimensions is now another name for the 3 tuple of Integers and supports all tuple functions.

Lets start by finding the paper needed for one box.
The paper needed is the surface area of the box plus the area of the smallest side.

First we find what the smallest side is
```
smallestSides :: Dimensions -> (Integer, Integer)
smallestSides (l,w,h) = arrayToTuple $ take 2 $ sort [l,w,h]
```

We're using `(l,w,h)` on the left hand side to destructure and give names to the contents of the tuple. We then put it in an array, sort in descending order, and the the largest two sides.

The `$` operator (a normal function, not special syntax sugar!) allows us to avoid parentheses. If you saw that and didn't know what it meant, you could have opened the ghci repl program and typed `:t $`. It would tell you it's type is `($) :: (a -> b) -> a -> b`. It is an infix operator where the left hand side is the function, and the right hand side is the argument to that function. You can think of this as "insert an open parentheses at the dollar sign, and a closed parentheses all the way at the end".

The following function is equivalent:
```
smallestSides :: Dimensions -> (Integer,Integer)
smallestSides (l,w,h) = arrayToTuple (take 2 (sort [l,w,h]))
```

Next we find the surface are of the box.
```
boxSurfaceArea :: Dimensions -> Integer
boxSurfaceArea (l,w,h) = 2*l*w + 2*w*h + 2*h*l
```

Then we just add them together to get the amount of paper needed for one box.
```
paperAmount :: Dimensions -> Integer
paperAmount d = boxSurfaceArea d + smallestSidesArea d
```

Like before, once we've found the area needed for one element, its easy to sum all of them.

```
totalPaperAmount :: [Dimensions] -> Integer
totalPaperAmount = foldl' (\acc x -> acc + paperAmount x) 0
```

We're using an anonymous method with the `(\acc x -> ...)` notation instead of writing an explicit step function this time. Part 1 is solved.

### Part 2
For the next problem we need to find the bow length, which is the perimeter of the smallest side plus the cubic volume of the present. The perimeter of a rectangle is `2 * length + 2 height`. We can reuse our `smallestSides` function from earlier.
```
smallestSidesPerimeter :: Dimensions -> Integer
smallestSidesPerimeter d = let (f,s) = smallestSides d in 2*f + 2*s
```

Finding the volume is easy
```
boxVolume :: Dimensions -> Integer
boxVolume (l,w,h) = l*w*h
```

We just add these together to get the ribbon length for one box
```
ribbonLength:: Dimensions -> Integer
ribbonLength d = smallestSidesPerimeter d + boxVolume d
```

and fold over all the boxes to find our total ribbon length.

## [Day 3: Perfectly Spherical Houses in a Vacuum ](http://adventofcode.com/day/3)
Day 3 has us reading a list of directions for Santa to move, and counting the number of unique houses he visits. This one isn't quite as simple as the first two, but since we're still taking a list of data and reducing it to a single value, we'll use `fold` again.

Let's start by thinking about our data. We'll treat the houses as points on an X,Y coordinate grid.
```
type Position = (Integer, Integer)
```
We're only interested in unique houses he visited, so we'll keep track of positions he visits in a `Set`

```
type HousesVisited = Set Position
```

We need to keep track of where he is currently, as well as the houses he's visited so far, so we'll make another type to represent his current state.

```
type SantaState = (HousesVisited, Position)
```

We'll treat his initial state as (0,0)
```
initialState :: SantaState
initialState = (S.singleton (0,0), (0,0))

```

We'll make a sum type to represent the directions
```
data Direction = North | South | East | West
```

and a simple type constructor to convert the input. The `case` syntax can be thought of a C style `switch` on steroids.

```
toDirection :: Char -> Direction
toDirection c = case c of
  '^' -> North
  'v' -> South
  '>' -> East
  '<' -> West
```

When the elves radio in a new direction we want to update his position
```
nextPosition :: Position -> Direction -> Position
nextPosition (x,y) d = case d of
  North -> (x, y+1)
  South -> (x, y-1)
  East  -> (x+1, y)
  West  -> (x-1, y)
```

Next we'll put this all together in our step function, which takes a direction, updates his position, and adds his new position to set of all the houses he has visited so far.

```
changePosition :: SantaState -> Direction -> SantaState
changePosition (visited,pos) dir = (insert nextPos visited, nextPos) where
  nextPos = nextPosition pos dir
```
 Now we just fold over all the directions input to find his final position, and a set of all the unique positions he has visited. The `.` operator is function composition `f (g x) = (f . g) x`
 just like you learned in math class.
```
housesVisited :: String -> SantaState
housesVisited = foldl changePosition initialState . map toDirection
```

At the end we just ask for the size of the set to get our solution.
```
let (santaHouses, end) = housesVisited $ santaList input
print $ size santaHouses
```

### Part 2
Part two asks us to find the total number houses that Santa and Robot Santa visit. They take turns responding to the elves input. To solve this we'll just split up the input into two lists instructions, giving robot santa every other instruction.

```
santaList :: [a] -> [a]
santaList (a:b:rest) = a:santaList rest
santaList (a: _ ) = [a]
santaList _ = []

robotList:: [a] -> [a]
robotList (a:b:rest) = b:robotList rest
robotList  _ = []
```

We can then use the exact same logic as before, then take the union of both the sets, and find its size.


```
let (santaHouses,end) = housesVisited $ santaList input
let (robotHouses,end) = housesVisited $ robotList input
print $ S.size $ S.union santaHouses robotHouses
```

## [Day 4: The Ideal Stocking Stuffer](http://adventofcode.com/day/4)
This problem asks us to find the first number, which when appended to your secret key, produces an MD5 hash starting with five zeroes.

Let's start with the little bit of boilerplate. We'll make a simple function to append a number to our secret string.

```
appendNumberToSecret :: Int -> ByteString -> ByteString
appendNumberToSecret n s = concat [s, pack $ show n]
```

We'll use an MD5 hashing library to hash this value. We append `:: Digest MD5` to the `hash` function to get an MD5 hash and then use `digestToHexByteString` to convert this into a string representation.

```
hashSecret :: Int -> ByteString -> ByteString
hashSecret n s = digestToHexByteString (hash $ appendNumberToSecret n s :: Digest MD5 )
```

Now let's move on to the interesting part. We'll create an infinite list of our secret hashed with all integers. `[0 .. ]` creates an infinite list of integers. We combine this in a tuple with the original integer that created the hash, since this is value we're looking for in the end

```
allHashes :: ByteString -> [(Int, ByteString)]
allHashes s = map (\n -> (n, hashSecret n s)) [0 ..]
```

We're looking for a hash that starts with 5 zeros, so let's make a generic function that checks for this.

```
startsWithNZeros :: Int -> ByteString -> Bool
startsWithNZeros n = isPrefixOf $ replicate n '0'
```

Now we can clearly describe what we want. We'll take our infinite list of hashes, filter only the ones that start with five zeros, take the first one, and then get the corresponding integer (the first element in the tuple) that when appended to our secreted, generated the proper hash. Haskell is a lazy language, so it won't actually generate infinite hashes, it will only generate what we need.

```
getNumber :: Int -> ByteString -> Int
getNumber numZeros = fst . head . filter (startsWithNZeros numZeros . snd) . allHashes

main = print $ getNumber 5 "yzbqklnj"
```

### Part 2
Part two just asks us to find the first number that creates a hash starting with 6 zeros. It's easy to reuse our code for this.

```
main = print $ getNumber 6 "yzbqklnj"
```


## [Day 6 - Doesn't He Have Intern-Elves For This?](http://adventofcode.com/day/5)
Day 6 has us counting the number of strings that match certain properties. This is mostly a Regex problem but I won't go super into detail on the expressions themselves. They key to solving these is to take advantage of a Regex feature called backtracking, which is using the result of one capture group later on in an expression.

Lets write some Regex tests for the properties they ask for. The `[re|*]` syntax is template Haskell which allows us to write Regex and get compile time checking. It is a DSL that makes sure we write valid Regex.

```
atLeastThreeVowels ::  String -> Bool
atLeastThreeVowels x = x =~ [re|(.*[aeiou]){3}|]

twoConsecutiveLetters :: String -> Bool
twoConsecutiveLetters x = x =~ [re|(.)\1|]

doesNotContainLetters :: String -> Bool
doesNotContainLetters x = not $ x =~ [re|(ab|cd|pq|xy)|]

twoLettersAppearTwice :: String -> Bool
twoLettersAppearTwice x = x =~ [re|\b\w*?(\w{2})\w*?\1\w*?\b|]

letterSandwich :: String -> Bool
letterSandwich x = x=~ [re|(.).\1|]
```

We'll make a simple utility function where we can see if a given string passes all given tests'

```
passesTests :: [String -> Bool] -> String -> Bool
passesTests ts i = all (\t -> t i) ts
```

We'll use this to create tests for Part 1 and Part 2
```
part1Test :: String -> Bool
part1Test = passesTests [atLeastThreeVowels, twoConsecutiveLetters, doesNotContainLetters]

part2Test :: String -> Bool
part2Test = passesTests [twoLettersAppearTwice, letterSandwich]
```

Then we just filter out the strings that pass the tests and count them.

```
print $ length $ filter part1Test (lines file)

print $ length $ filter part2Test (lines file)
```
## [Day 6: Probably a Fire Hazard](http://adventofcode.com/day/6)
In this question we need to turn on lights on a big 2D grid based on Santa's instructions. We're going to use mutable data structures to make this answer as fast as possible, although it will be a bit more complicated than the other answers. We'll see that when we really care about performance, we can manually manage mutable data structures in a purely functional language. We'll be making more use of some concepts like `Monad` and `Applicative` in the next two examples, which I'll point out, but will gloss over since they're beyond the intended scope of this post.

Let's start by writing a parser for Santa's instructions. We'll use the excellent Attoparsec library. We can build complex parsers out of small, easy to reason about components.

Santa's instructions have the form `turn off 499,499 through 500,500`
Let's make some data types to represent what we need.
```
data LightAction = TurnOn | TurnOff | Toggle deriving Show

type Point = (Int,Int)

data Instruction = Instruction { action :: LightAction,
                                 from   :: Point,
                                 to     :: Point
                               } deriving Show
```

The instructions in general have the form `Instruction` `space` `Point` ` through ` `Point`

`Point` as the form `Digit` `comma` `Digit`

First the instruction parser:

```
lightActionParser :: Parser LightAction
lightActionParser = (string "turn on"  >> return TurnOn)
                <|> (string "turn off" >> return TurnOff)
                <|> (string "toggle"   >> return Toggle)
```
If the string matches, it will return its corresponding LightAction. The `<|>` operator tries each of these in order, returning the first one that succeeds.

Next the point parser:

```
pointParser :: Parser Point
pointParser = do
  x <- decimal
  char ','
  y <- decimal
  return (x,y)
```
This parser takes a number, consumes the `,` and throws it away, then takes the next number, and finally turns it into a point. This `do` notation is easy to understand coming from an imperative background and is readable, but there's an alternative way of writing this in an `Applicative` style, which is a more functional approach. I don't want to get into explaining `Functor`, `Applicative`, and `Monad` so I'll give a hand wavey example of this style. A good explanation with pictures can be found [Here](http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html).

This is equivalent to above:
```
pointParser2 :: Parser Point
pointParser2 = (,) <$> (decimal <* char ',')
                   <*> decimal
```
`(,)` is the point type constructor which takes two more parameters and returns a Point. The `decimal <* char ','` reads the decimal and throws away the `,`. I prefer this style because it doesn't require any intermediate values and is more composable than `do` notation, but this is somewhat up to personal preference. When you get more comfortable with Applicative Functors and your library's operators you may prefer this style as well.

Now lets combine our point and action parser.

```
parseInstruction :: Parser Instruction
parseInstruction = do
  a <- lightActionParser
  space
  p1 <- pointParser
  string " through "
  p2 <- pointParser
  return $ Instruction a p1 p2
```

and in Applicative style:

```
parseInstruction2 :: Parser Instruction
parseInstruction2 = Instruction <$> (lightActionParser <* space)
                                <*> (pointParser <* string " through ")
                                <*> pointParser
```

We can then easily turn this into a parser that parses all the instructions in the file.

```
parseAllInstructions :: Parser [Instruction]
parseAllInstructions = many $ parseInstruction <* endOfLine
```

Haskell is great at parsing and doesn't make this any harder than it needs to be. Let's move on to the actual problem.
Once again I'm going to gloss over some details so feel free to just skim this.

We get two points that represent a square of lights and an action to perform on them. In an imperative language we could use a 2d array of bools and a nested for loop to turn on all the lights in a square. If x1,y1 is the starting point and x2,y2 are the ending points it would look something like this:
```
for (int x = x1; x < x2; x++){
  for(int y = y1; y < y2; y++ ){
    allLights[x][y] = true;
  }
}
```

Here's what we'll do in Haskell where a is the `LightAction` and
```
updateWithInstruction allLights ins = do
--- some boilerplate removed...
 forM_ [x1 .. x2 ] $ \x ->
   forM_ [ y1 .. y2 ] $ \y ->
     handleAction allLights (x * yardWidth + y) a
```
Instead of using a 2d array, I'm using a 1D array of bools offset by the yard width (1000).

We'll use a function to mutate the vector depending on the action. `instance HandleLightAction Bool` means our program will run this method when our `allLights` vector contains `Bools`. This is roughly equivalent to implementing an interface.
```
instance HandleLightAction Bool where
  handleAction lights idx ins = do
    case ins of
      TurnOn  -> VM.write lights idx True
      TurnOff -> VM.write lights idx False
      Toggle  -> VM.modify lights not idx
    return lights  
```

We'll then take every instruction, and perform the proper grid mutations:
```
forM_ allInstruction $ updateWithInstruction allLights
```

To find how many lights are turned on at the end, we just filter out only True values and count them.
```
print $ V.length $ V.filter id yard
```

Part two asks us to dim and brighten the lights instead of turning them on and off. I made `handleAction` a type class method, so our program knows to turn them on and off if yard is a vector of `Bool`s and to dim and brighten if it's a vector of `Int`s. I also made a `dim` utility method.

```
instance HandleLightAction Int where
  handleAction lights idx ins = do
    case ins of
      TurnOn  -> VM.modify lights (+1) idx
      TurnOff -> VM.modify lights dim idx
      Toggle  -> VM.modify lights (+2) idx
    return lights

dim :: Int -> Int
dim i = if i - 1 < 0 then 0 else i - 1
```

At the end we just sum the vector to find our total brightness.
```
print $ V.sum yard
```
I've left out the type signatures and the HandleLightAction typeclass because they're a little too detailed for this post, but feel free to look in the source code.

## [Day 7 - Some Assembly Required](http://adventofcode.com/day/7)

Day 7 asks us to build a circuit out of a box containing wires and bitwise logic gates. Let's start with our data. Our circuit will either be a signal, a wire with a label (which will provide a circuit from the box), or one of the specified bitwise gates with a circuit input. Our goal is to find the output signal of a given wire.

```
newtype Label = Label Text deriving (Show, Ord, Eq)

data Circuit = Signal Word16
             | Wire Label
             | AndGate Circuit Circuit
             | OrGate Circuit Circuit
             | LShiftGate Int Circuit
             | RShiftGate Int Circuit
             | NotGate Circuit deriving (Eq, Show, Ord)
```
We'll represent our box of circuits as a Map with a `Label` key and a `Circuit` Value. We'll skip writing the parser in this section, but feel free to look at the source.

```
type CircuitBox = M.Map Label Circuit
```

Now lets define our function that will tell us the output signal of a given `Circuit`.

```
runCircuit :: CircuitBox -> Circuit -> Word16
```

If the provided `Circuit` is a `Signal`, we already know the output signal and just return it. We don't even need to look in the `CircuitBox`.
```
runCircuit _ (Signal a) = a
```

If the `Circuit` is a `Wire`, we search in the `CircuitBox` for the `Circuit` that feeds into that `Wire` and recursively run our function with the newfound `Circuit` until we encounter the input signal.

```
runCircuit b (Wire a) = runCircuit b $ (M.!) b a
```

If the `Circuit` is a gate, we use the corresponding bitwise operator. `(.&.)` is bitwise AND and `(.|.)` is bitwise OR.
```
runCircuit b (AndGate r l) = (.&.) (runCircuit b r) (runCircuit b l)
runCircuit b (OrGate r l) = (.|.) (runCircuit b r) (runCircuit b l)
runCircuit b (LShiftGate i a) = shiftL (runCircuit b a) i
runCircuit b (RShiftGate i a) = shiftR (runCircuit b a) i
runCircuit b (NotGate a) = complement (runCircuit b a)
```

To find the solution we request the `Signal` for the given `Wire` and pass in the `CircuitBox` that we parsed from the input file.

```
runCircuit circuitBox (Wire (Label "a"))
```

This program runs correctly but there's a problem: it's extremely slow. Let's figure out how to make it fast.
Reddit user [Borkdude](https://www.reddit.com/user/Borkdude) made a graph of his circuit input for this problem.

![Day 7 Graph](https://github.com/brsunter/AdventOfCodeHaskell/blob/master/res/day7graph.png)

If we're looking for the signal of wire a (at the top) we can see its dependency graph underneath it. We can see that both ls and lr depend on the value from lq. This means that our program will compute the entire dependency graph of lq two times, which is a lot of uneccesary work. We need to memoize our function, so after a circuit as been computed once, it will return instantly for future calls with the same input.

Haskell has a Memo library that helps us do this. Our new `runCircuit` function will use the Memo Monad

```
runCircuit :: CircuitBox -> Circuit -> Memo (CircuitBox, Circuit) Word16 Word16
```

`Memo (CircuitBox, Circuit) Word16 Word16` means that we will memoize functions that `CircuitBox` and `Circuit` parameters which return a Word16 result. The Word16 at the end means we return Word16 (the requested `Signal` value).

We rework our function to use `for2 memo` which memoizes a function with two arguments. We then alter our `runCircuit` function in applicative style using `<$>`.


```
runCircuit :: CircuitBox -> Circuit -> Memo (CircuitBox, Circuit) Word16 Word16
runCircuit b (Signal a) = return a
runCircuit b (Wire a) = for2 memo runCircuit b $ (M.!) b a
runCircuit b (AndGate r l) = (.&.) <$> for2 memo runCircuit b r <*> for2 memo runCircuit b l
runCircuit b (OrGate r l) =  (.|.) <$> for2 memo runCircuit b r <*> for2 memo runCircuit b l
runCircuit b (LShiftGate i a) = flip shiftL i <$> for2 memo runCircuit b a
runCircuit b (RShiftGate i a) = flip shiftR i <$> for2 memo runCircuit b a
runCircuit b (NotGate a) = complement <$> for2 memo runCircuit b a

```
Now our function will instantly return the correct values for any circuit that has already been computed. To run our function we use
`startEvalMemo $ runCircuit circuitBox (Wire (Label "a"))` which quickly solves the problem.
