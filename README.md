# Spooky Melbourne Housing Explorer 🎃

An interactive Shiny app for exploring Melbourne housing prices by region, property type, and home features.

## Purpose

This app lets users explore home sales in Melbourne, Australia from 2016 to 2018. Users can filter the data, view and download it, and create their own tables and plots to see how prices relate to location, property type, and home features. It was built as a project for ST 558 and since the project was due on Halloween week, a Halloween theme was used for fun.

## Data

The data comes from the [Melbourne Housing Market dataset on Kaggle](https://www.kaggle.com/datasets/anthonypino/melbourne-housing-market). Each row is one home sale. Sales without a recorded price and a few impossible build years were removed during cleaning.

## App Features

- **Sidebar:** filter by property type, region, and ranges for two numeric variables, then click Apply Filters
- **About tab:** describes the app, the data, and how to use each section
- **Data Download tab:** allows the user to view the filtered data in a table and download it as a CSV
- **Data Exploration tab:** creates one-way and two-way contingency tables, numeric summaries across categories, and bar charts, histograms, boxplots, scatter plots, and heatmaps with options for coloring and faceting

## Live App

[View the app on shinyapps.io](LINK_GOES_HERE)

