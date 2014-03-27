{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, NoRecordWildCards, DeriveGeneric #-}

module Keter.Node where

-- Control Libraries 
import CorePrelude



-- Local Libraries
import Keter.Node.Internal
import Keter.Node.Types




{-|

keter-node is designed to extend the functionality of keter to allow the same binary to be ran multiple times in a given keter directory under different port configurations.  This allows you to spawn instances of a binary as 'nodes' and then let the spawning agent know what the port and process are for future reference 


I will try to borrow as many types from keter directly, and not create too much additional bagage, but this is a library designed for middleware so there will be some serialization parts. 

|-}
