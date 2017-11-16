&nbsp;     
&nbsp;     
&nbsp;     
&nbsp;     
&nbsp;     
&nbsp;     
&nbsp;     
&nbsp;     
&nbsp;     
&nbsp;     

Kleisli Categories
==================

Logger

    string logger
    bool negate(bool b) {
        logger += "Not so! ";
        return !b;
    }

We want to simpulate write only side effects without global state

    negate (b, logger) = (!b, logger ++ "Not so! ")

- Not so good design
- Idea: Only return new log entry, aggregate log between calls

    negate (b) = (!b, "Not so" ") 

Ad hoc Composition
------------------

    toUpper s = ...
    toWords s = ...

type Writer a = (a, String)

    toUpper s = (..., "toUpper ")
    toWords s = (..., "toWords")

    process s = 
        let p1 = toUpper s
            p2 = toWords (fst p1)
            in (fst p2, snd p1 ++ snd p2) 
    
    or

    process s = 
        let (result1, logger1) = toUpper s
            (result2, logger2) = toWords result1
            in (result2, logger1 ++ logger2) 
    

    process "foo bar" = (["FOO", "BAR"], "toUpper toWords ")


Better Composition
------------------

Fish = compose any two functions returning a Writer
    (>=>) :: (a -> Writer b) -> (b -> Writer c) -> (a -> Writer c)

    (>=>) f1 f2 x =
        let (result1, logger1) = f1 x
            (result2, logger2) = f2 result1
            in (result2, logger1 ++ logger2) 

    process s = (toUpper >=> toWords) s



Kleisli Categories
------------------

"Writer a" is called an embellishment of a

- Category based on a monad (??)
- Objects are types
- Morphism from A to B represents functions from A to the embellished B

The writer category is a Kleisli Category

Identity morphism represents a function from A to the emellsihed A

    identity x = (x, "")

