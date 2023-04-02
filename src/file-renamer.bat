@echo off
ghc -package random-shuffle -o file-renamer.exe Main.hs
file-renamer.exe %*
