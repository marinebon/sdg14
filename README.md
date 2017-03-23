# sdg14
Products for Sustainable Development Goal 14 on Life in the Sea from MBON (Marine Biodiversity Observation Network)

## Background

This repository was setup to version and collaborate code and small files subsequent to the March 15-17 meeting in St Petersburg, FL. For notes, presentations and references from the meeting see [bit.ly/sdg14mbon](http://bit.ly/sdg14mbon).

## Folder Organization

Subfolders are currently organized by teams:

1. biodiversity
1. infographics
1. satellite
1. technical

## Large Files on Server

Github limits free, public repositories to 1 GB total with a per file limit of 100 MB (and warning > 50 MB). Any files larger than 50 MB, I strongly recommend storing on our server `mbon.marine.usf.edu` in the `/mbon/data_big` folder. 

```
├── data_big
│   ├── biodiversity
│   ├── infographics
│   ├── satellite
│   └── technical
└── sdg14
    ├── biodiversity
    ├── infographics
    ├── satellite
    │   ├── data_small
    │   │   └── climate_indices
    └── technical
```

Note how the sdg14 Github repo is cloned and available to update (`git pull`; `git commit; git push`).

To get an account on the server, please fill out your info on this spreadsheet:

  [Github & mbon.marine.usf.edu accounts - Google Sheets](https://docs.google.com/spreadsheets/d/1KEH0dyjQemg_Tfik0dIIyQ30cqQ1eRcWTaMuZwUO49U/edit#gid=0)

## Software Setup

For a quick primer on getting setup with Git, Github and RStudio, please see:

  [marinebon.github.io/help](https://marinebon.github.io/help/setup.html)

We will be expanding documentation here in the near future.
