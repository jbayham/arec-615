# Presentation chooser

This small base-R tool randomly selects presenters while gently favoring students who have not presented recently.

## Set up the roster

Edit `roster.csv` so it has exactly one column named `name`, with one unique student name per row. The supplied names are examples only.

```csv
name
Student One
Student Two
```

## Choose presenters

At the beginning of class, from this directory, run (for example):

```sh
Rscript ~/Documents/git_projects/arec-615/presentation_chooser/choose_presenters.R 3
```

The number is how many presenters to select. The script prints the selection and stores it in `presentation_history.csv`. Re-running the same command on the same calendar date prints the saved result instead of drawing again.

Each student begins with equal probability. After each presentation day a student is not selected, their sampling weight rises by 10%. A student selected on the most recent presentation day has weight 1.0; a student who missed two presentation days has weight 1.2.

## Replace an absent presenter

If a selected student is absent, run:

```sh
Rscript ~/Documents/git_projects/arec-615/presentation_chooser/choose_presenters.R 3 --replace "Student Name"
```

The count is still required for consistency, but the command draws exactly one replacement. It removes the absent student from that day's saved result, so they are not recorded as having presented. The replacement cannot duplicate another presenter already selected that day.

## Run tests

```sh
Rscript tests/test_chooser.R
```
