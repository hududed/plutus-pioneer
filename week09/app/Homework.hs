{-# LANGUAGE OverloadedStrings #-}

module Example where

import           Language.Marlowe.Extended

main :: IO ()
main = print . pretty $ contract


{- Define a contract, Close is the simplest contract which just ends the contract straight away
-}

alice, bob, charlie :: Party
alice = "Alice"
bob = "Bob"
charlie = "Charlie"

deposit :: Value
deposit = Constant 10

choiceId :: ChoiceId
choiceId =  ChoiceId "winner" charlie


contract :: Contract
contract =
    When
        [ f charlie alice bob
        , f charlie bob alice
        ]
        10 Close
    where
     f :: Party -> Party -> Party -> Case
     f x y z = 
        Case
            (Deposit
                x 
                x
                ada
                (Constant 20)
            )
            (When
                [Case
                    (Deposit
                        y
                        y
                        ada
                        deposit
                    )
                    (When
                        [Case
                            (Deposit
                                z
                                z 
                                ada
                                deposit
                            )
                            (When
                                [Case
                                    (Choice
                                        choiceId
                                        [Bound 1 2]
                                    )
                                    (If
                                        (ValueEQ
                                            (ChoiceValue choiceId)
                                            (Constant 1)
                                        )
                                        (Pay
                                            bob
                                            (Account alice)
                                            ada
                                            deposit
                                            (Pay
                                                charlie
                                                (Account charlie)
                                                ada
                                                (Constant 20)
                                                Close
                                            )
                                        )
                                        (Pay
                                            alice
                                            (Account bob)
                                            ada
                                            deposit
                                            (Pay
                                                charlie
                                                (Account charlie)
                                                ada
                                                (Constant 20)
                                                Close
                                            )
                                        )
                                    )]
                                40
                                (Pay
                                    charlie
                                    (Account alice)
                                    ada
                                    deposit
                                    (Pay
                                        charlie
                                        (Account bob)
                                        ada
                                        deposit
                                        Close
                                    )
                                )
                            )]
                        30 Close
                    )]
                20 Close
            ) 
