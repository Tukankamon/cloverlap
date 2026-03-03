This program takes a list of possible classes you might want to sign up for next semester in the format show in the example.csv. It then calculates the most optimal schedule for you depending on how much rest you want between classes and exams, how many classes total you want to take and sorts by how much downtime between classes each schedule has. This results in a schedule that doesnt have classes or final exams that overlap and makes the best of your time so you dont have to spend 4 hours at university killing time because you live far away

# CSV syntax
As shown in the example.csv. Dates are set either with a weekday or with a day/month, both followed by the start and end times

Empty dates for exams or classes are not parsed (only implemented for time3 and exam3 for now)

# Usage
```IO
Usage: class [-v|--verbose] [--class-rest INTEGER] [--exam-rest INTEGER] 
             [-M|--max-classes INTEGER] [-m|--min-classes INTEGER]

  Compute the most optimal class schedule from a list of courses

Available options:
  -v,--verbose             Whether to enable verbose mode when printing out data
  --class-rest INTEGER     How much time in minutes minimum in between classes
                           (default: 10)
  --exam-rest INTEGER      How much time in days minimum in between exams
                           (default: 1)
  -M,--max-classes INTEGER Maximum classes to sign up for, this includes those
                           that wont be attended to but will self study to go to
                           the exam (default: 8)
  -m,--min-classes INTEGER Absolute minimum amount of classes to attend
                           (default: 6)
  -h,--help                Show this help text
```

# Nix (flakes)
The program is availabe to run with nix. To do this run:
```nix
nix run <github url>
```
To add it to your system configuration add the flake in the repo to your own flake.nix
