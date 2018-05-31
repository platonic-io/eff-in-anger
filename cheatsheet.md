# Cheatsheet

1. Start from dummy main and imports/pragmas prefilled
2. let's write a REST server to manage pets -> main with port, call startServer
3. startServer run port application
4. write application
5. decompose runServer into runServant + runEFf
6. implement handlers as pure ()
7. compile and run the app
9. write a test  -> addPet returns [thePet]
8. model business logic as an Eff -> BusinessLogicEffect -> AddPet precanned answer
9. write a test to add a second Pet -> fails, we need a store
10. model the store using an MVar + map of ByteString with encode
11. thread the Store in runEff
12. change tests to return a Right -> we want to handle errors in Storage
13. write a test for failing store
14. implement filaing store as an alternative effect
