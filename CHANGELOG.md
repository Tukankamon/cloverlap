# Revision history for class

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.

# TODO
There are a lot of suggestions in the code tagged with `#TODO` for more things to do
- [ ] Adress performance issues on larger sets (use of subsequences in Optimize.hs)
- [ ] Make distinct examblocks and classblocks
- [ ] Improve formatting in verbose mode
- [x] Add user input
- [ ] Be able to have null if you dont need 3 class periods
- [x] Add the actual logic
    - [x] Check that Classes (not marked with skip_class) and Exams dont overlap
    - [x] Have a min_rest variable for minimum rest between classes
        - [x] Same but for exams
    - [x] Minimize downtime between classes
    - [x] Priority indicator for when there is a max allowed classes
- [x] Add user input for rest times and amount of classes
- [ ] Add something like a TUI to make user input of the csv easier
    - [ ] Or even better GUI with GTK (has haskell integration)
