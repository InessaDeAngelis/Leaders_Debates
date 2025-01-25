#### Preamble ####
# Purpose: Sentiment analysis of YouTube comments for Chapter 5
# Author: Inessa De Angelis
# Date: 21 January 2025
# Contact: inessa.deangelis@mail.utoronto.ca 
# License: MIT

## Load libraries ##
import pandas as pd
from vaderSentiment.vaderSentiment import SentimentIntensityAnalyzer

# Create function to compute sentiment scores ##
def sentiment_scores(sentence):
    ## Create a SentimentIntensityAnalyzer object ##
    sid_obj = SentimentIntensityAnalyzer()

    ## Get sentiment scores ##
    sentiment_dict = sid_obj.polarity_scores(sentence)
    
    return sentiment_dict

## Create function to read comments from a CSV, analyze sentiment, and save to a new CSV
def analyze_comments(input_csv, output_csv):
    ## Read comments ##
    try:
        df = pd.read_csv(input_csv)
    except FileNotFoundError:
        print(f"Error: The file '{input_csv}' was not found.")
        return
    except pd.errors.EmptyDataError:
        print("Error: The file is empty.")
        return

    ## Check if the CSV has a "comment" column ##
    if 'comment' not in df.columns:
        print("CSV file must have a 'comment' column.")
        return

    ## Deal with missing values or non-string entries in the 'comment' column ##
    df['comment'] = df['comment'].fillna('')  
    df['comment'] = df['comment'].astype(str)  

    ## Analyze sentiment for each comment and save the results ##
    sentiment_results = []

    for index, row in df.iterrows():
        comment = row['comment']
        
        # Check if the comment is empty ##
        if not comment.strip():
            sentiment_results.append({
                'negative': None,
                'neutral': None,
                'positive': None,
                'compound': None,
                'sentiment': 'Neutral'
            })
        else:
            ## Analyze sentiment for non-empty comments ##
            sentiment_dict = sentiment_scores(comment)
            sentiment_results.append({
                'negative': sentiment_dict['neg'],
                'neutral': sentiment_dict['neu'],
                'positive': sentiment_dict['pos'],
                'compound': sentiment_dict['compound'],
                'sentiment': 'Positive' if sentiment_dict['compound'] >= 0.05 else ('Negative' if sentiment_dict['compound'] <= -0.05 else 'Neutral')
            })

    ## Convert results to df ##
    sentiment_df = pd.DataFrame(sentiment_results)

    ## Add new results back with old df ##
    df = pd.concat([df, sentiment_df], axis=1)

    ## Save to CSV ##
    df.to_csv(output_csv, index=False)
    print(f"Sentiment analysis results saved to {output_csv}")

## CSV info ##
if __name__ == "__main__":
    input_csv = 'Outputs/Data/YouTube/all_debate_comments.csv'  # Input CSV with comments
    output_csv = 'Outputs/Data/YouTube/debate_comments_sentiment_scores.csv'  # Output CSV with sentiment results
    
    analyze_comments(input_csv, output_csv)
