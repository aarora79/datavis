import requests as req
import logging
import time
import importlib
import os
import json
import pandas as pd
import tweepy
import argparse
import sys
import codecs
import nltk
from nltk.corpus import stopwords
from nltk.stem import WordNetLemmatizer
import re
from wordcloud import WordCloud,STOPWORDS
from scipy.misc import imread
import matplotlib.pyplot as plt
import string

#globals
MAX_TWEETS_ALLOWED         = 30
MAX_TWEETS_ALLOWED_DEFAULT = 30
N_FOR_TOP_N_WORDS_DEFAULT  = 100
HASHTAG_DEFAULT            = "#ggplot2"
STOPWORDS_DEFAULT          = ["b'rt", "\\n", "\\n\\n", "..."] 
STOP_WORDS_FILE_DEFAULT    = "stopwords.txt"

def _set_logger():
    """Sets up the logging for this module. Assumes that there is a global variable 
       called "logger" already defined and sets it up with formatted logging.
       Logs are sent to the console as well as a file (file name is <name of this source
       file.log>).

       Parameters
       ---------- 
       
       Returns
       ----------

    """
    global logger
    #first completely reset any existing logging (usefuwhen this code is running in a notebook)
    logging.shutdown()
    importlib.reload(logging)

    
    try:
        log_fname = os.path.basename(__file__)[:-3] + '.log'
        print(log_fname)
    except Exception as e:
        log_fname = "api.log"
        
    logger = logging.getLogger(os.path.basename(log_fname))
    logger.setLevel(logging.DEBUG)
    ch = logging.StreamHandler()
    ch.setLevel(logging.DEBUG)
    fileHandler = logging.FileHandler(log_fname, mode='w')
    fileHandler.setLevel(logging.DEBUG)
    formatter = logging.Formatter('%(asctime)s - %(levelname)s - %(message)s')
    ch.setFormatter(formatter)
    fileHandler.setFormatter(formatter)
    logger.addHandler(ch)
    logger.addHandler(fileHandler)
    formatter = logging.Formatter('%(asctime)s - %(levelname)s - %(message)s')


def _check_config(config):
    logger.info("input parameters are as follows")
    logger.info(json.dumps(config, indent=2))
    #check of the # is there in the #hashtag and clamp count to 30 if greater
    config_updated = False
    if(not config['hashtag'].startswith('#')):
        logger.info("input hashtag did not start with #, adding it")
        config['hashtag'] = '#' + config['hashtag']
        config_updated = True
    
    if(config['count'] > MAX_TWEETS_ALLOWED):
        logger.info("input count >  %d, clamping count to %d" %(MAX_TWEETS_ALLOWED, MAX_TWEETS_ALLOWED))
        config['count'] = MAX_TWEETS_ALLOWED
        config_updated = True

    if(config['count'] <= 0):
        logger.info("input count %d is invalid, setting count to %d" %(config['count'], MAX_TWEETS_ALLOWED_DEFAULT))
        config['count'] = MAX_TWEETS_ALLOWED_DEFAULT
        config_updated = True

    if config_updated == True:
        logger.info("updated configuration is...")
        logger.info(json.dumps(config, indent=2))
    return config
        
def _get_tweets(config):
    consumer_key = 'TPkpWrTFiXLXdMruJiqLqnpxq'
    consumer_secret = 'Xmr5sLtFKNGWLOPbBvXcx3GuE9P5dV1Aek7NDLgPD9qf9XLptV'
    access_token = '105755342-DJRZot7RP7jEpiVBbC15r0IF2fhgUsM0Vm8OKj7b'
    access_token_secret = 'Q5LQFguR4ISb6spYi22fqfi65S8AdyvDXXCwIRE9eN5EK'
    
    auth = tweepy.OAuthHandler(consumer_key, consumer_secret)
    auth.set_access_token(access_token, access_token_secret)
    api = tweepy.API(auth)

    fname = config['fname']
    with open(fname, 'w') as raw_tweets:
        for tweet in tweepy.Cursor(api.search, q=config['hashtag'] + " -filter:retweets").items(config['count']):
            #logger.info(json.dumps(vars(tweet), indent=2))
            logger.info(str(tweet.text).encode('utf-8'))
            raw_tweets.write(str(str(tweet.text).encode('utf-8')))
            raw_tweets.write("\n")
    
def _process_tweets(config):
    #load default stop words
    default_stopwords = set(nltk.corpus.stopwords.words('english'))

    #custom stop words, read from a file file but load some hardcoded ones also
    #we have to use this codecs stuff because twitter data is definitely not ascii
    custom_stopwords = set(STOPWORDS_DEFAULT)
    if os.path.exists(config['stopwordsfile']):
        custom_stopwords = custom_stopwords | set(codecs.open(config['stopwordsfile'], 'r', 'utf-8').read().splitlines())

    all_stopwords = default_stopwords | custom_stopwords
    logger.info("stopwords list is as follows: ->")
    logger.info(all_stopwords)

    #open the tweets file contraining raw tweets
    fp = codecs.open(config['fname'], 'r', 'utf-8', errors='ignore')

    #we dont want urls in the text, so remove URLs
    cleaned_tweet_text = re.sub(r"http\S+", "", fp.read())

    #read the text and tokenize
    words = nltk.word_tokenize(cleaned_tweet_text)

    #remove single-character tokens (mostly punctuation)
    words = [word for word in words if len(word) > 1]

    #make everything lowercase
    words = [word.lower() for word in words]

    #non-ascii characters are printed as x80, x9f so remove all those
    #words = [w for w in words if not re.search('^x.*', w)]
    #words = [w for w in words if not w.startswith("\\x")]
 
    #still there could be words containing \x80, \x9f etc
    #we want to replace the \x80 etc with ""
    words = [re.sub(r'[^\x00-\x7f]',r'', codecs.decode(w, 'unicode_escape')) for w in words]

    #remove any words with no letters or numbers
    words = [word for word in words if re.search('[a-zA-Z0-9]', word) is not None]

    #remove punctuations
    translate_table = dict((ord(char), None) for char in string.punctuation)   
    words = [word.translate(translate_table) for word in words]

    #all set to remove stop words now
    words = [word for word in words if word not in all_stopwords]

    #lemmatize
    wordnet_lemmatizer = WordNetLemmatizer()
    words = [wordnet_lemmatizer.lemmatize(word) for word in words]

    #just use NLTK for frequency distribution
    freq_dist = nltk.FreqDist(words)

    #print only top N words
    words_freq = {'words':[], 'freq':[]}
    for word, frequency in freq_dist.most_common(config['n']):
        logger.info(u'{};{}'.format(word, frequency))
        words_freq['words'].append(word)
        words_freq['freq'].append(frequency)

    #store in a dataframe    
    df = pd.DataFrame(words_freq)
    #store the frequency distribution in a new file
    df.to_csv(config['wordfreqfnametableau'], index=False)
    df[["words", "freq"]].to_csv(config['wordfreqfname'], index=False, sep=":", header=False)


    # Display the image:
    bag_of_words =  ' '.join(words)
    wordcloud = WordCloud(max_font_size=40,stopwords=all_stopwords,background_color='white').generate(bag_of_words)
    wordcloud.to_file(config['wordcloudfilename'])


    twitter_mask = imread('twitter_mask.png', flatten=True)
    #logger.info(bag_of_words)
    wordcloud = WordCloud(
                         font_path='CabinSketch-Bold.ttf',
                         stopwords=all_stopwords,
                         background_color='white',
                         width=1800,
                         height=1400,
                         mask=twitter_mask
                         ).generate(bag_of_words)

    wordcloud.to_file(config['wordcloudfilename2'])

if __name__ == '__main__':

    #enable logging
    _set_logger()
    logger.info("initialized logger...")
    logger.info("Going to pull twitter data ..")
    
    #read configuration
    if not len(sys.argv) > 1:
        logger.info("no input arguments provided, going to use defaults, type \"python %s -h\" for help" %(os.path.basename(__file__)))
    parser = argparse.ArgumentParser()
    parser.add_argument("--hashtag", help="#hashtag for which to get the tweets, default is \"%s\"" %(HASHTAG_DEFAULT), default=HASHTAG_DEFAULT)

    help_text_for_count = "number of tweets to get for the given hashtag, default is %d, max allowed is %d" \
                          %(MAX_TWEETS_ALLOWED_DEFAULT, MAX_TWEETS_ALLOWED)
    parser.add_argument("--count", help=help_text_for_count, default=MAX_TWEETS_ALLOWED_DEFAULT)
    parser.add_argument("--stopwordsfile", help="filename for storing stopwords, default is %s" %(STOP_WORDS_FILE_DEFAULT),
                       default=STOP_WORDS_FILE_DEFAULT)
    parser.add_argument("--n", help="how many top words to include in the output, default is %d" %(N_FOR_TOP_N_WORDS_DEFAULT), 
                        default=N_FOR_TOP_N_WORDS_DEFAULT)
    args = parser.parse_args()
    config = vars(args)
    config['count'] = int(config['count'])
    config['fname'] = "tweets_for_#hashtag_%s.txt" %(config['hashtag'])
    config['wordfreqfname'] = 'word_freq_for_%s.txt' %(config['hashtag'])
    config['wordfreqfnametableau'] = 'word_freq_for_%s_tableau.txt' %(config['hashtag'])

    config['wordcloudfilename'] = 'wordcloud_for_%s.png' %(config['hashtag'])
    config['wordcloudfilename2'] = 'wordcloud2_for_%s.png' %(config['hashtag'])
    
    #check config, modify any params as needed
    config = _check_config(config)

    #get twitter data and store it in a file
    _get_tweets(config)

    #time to analyze it
    _process_tweets(config)

