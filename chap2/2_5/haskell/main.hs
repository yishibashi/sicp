import           Complex
import           Rational

main = do print "Rational <op> Rational"
          print $ ((makeRat 1 1) + (makeRat 3 2))
          print $ ((makeRat 1 1) - (makeRat 3 2))
          print $ ((makeRat 1 1) * (makeRat 3 2))
          print $ ((makeRat 1 1) / (makeRat 3 2))
          print "Complex <op> Complex"
          print $ ((fromRI 1 2) + (fromRI 10 (-2)))
          print $ ((fromRI 1 2) - (fromRI 10 (-2)))
          print $ ((fromRI 1 2) * (fromRI 10 (-2)))
          print $ ((fromRI 1 2) / (fromRI 10 (-2)))
          --print $ 2.2 + (fromRI 10.0 (-2.1))
          print "Complex (Int, Double) <op> Complex (Double, Double) "
          print $ ((fromRI 1 2.2) + (fromRI 10.0 (-2.1)))
          print $ ((fromRI 1 2.2) - (fromRI 10.0 (-2.1)))
          print $ ((fromRI 1 2.2) * (fromRI 10.0 (-2.1)))
          print $ ((fromRI 1 2.2) / (fromRI 10.0 (-2.1)))
          print "Complex<Rational, Rational> <op> Complex<Rational, Rational>"
          print $ (fromRI (makeRat 1 2) (makeRat 2 3)) + (fromRI (makeRat 5 2) (makeRat 4 3))
          print $ (fromRI (makeRat 1 2) (makeRat 2 3)) - (fromRI (makeRat 5 2) (makeRat 4 3))
          print $ (fromRI (makeRat 1 2) (makeRat 2 3)) * (fromRI (makeRat 5 2) (makeRat 4 3))
          print $ (fromRI (makeRat 1 2) (makeRat 2 3)) / (fromRI (makeRat 5 2) (makeRat 4 3))
          print "Int/Double <op> Rational"
          print $ 5 + (makeRat 3 2)
          print $ 5.1 - (makeRat 3 2)
          print $ 5.12 * (makeRat 3 2)
          print $ 5 / (makeRat 3 2)
          print "Int <op> Complex" -- These dont' work if fromInteger is not in 'instance Num Complex'
          print $ 7 + (fromRI 1.1 2)
          print $ 7 - (fromRI 1 2.1)
          print $ 7 * (fromRI 1.1 2.1)
          print $ 7 / (fromRI 1 2)
          print "Complex<Ratio, Ratio> <op> Complex<Double, Int>"
          print $ (fromRI (makeRat 1 2) (makeRat 2 3)) + (fromRI 5.2 3)
          print $ (fromRI (makeRat 1 2) (makeRat 2 3)) - (fromRI 5.2 3)
          print $ (fromRI (makeRat 1 2) (makeRat 2 3)) * (fromRI 5.2 3)
          print $ (fromRI (makeRat 1 2) (makeRat 2 3)) / (fromRI 5.2 3)

