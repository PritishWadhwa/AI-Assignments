from pyswip import Prolog
from nltk.corpus import stopwords
from nltk.tokenize import word_tokenize, sent_tokenize
from nltk import pos_tag
from nltk.sentiment import SentimentIntensityAnalyzer

sia = SentimentIntensityAnalyzer()

# import spacy

# nlp = spacy.load("en_core_web_sm")
# sw_spacy = nlp.Defaults.stop_words

sw = set(stopwords.words('english'))

def formatLists(l):
    ans = '['
    if len(l) != 0:
        ans += l[0]
        for i in range(1, len(l)):
            ans += (',' + l[i])
    return ans + ']'

with open("input.txt", mode = "r", encoding = "utf-8") as file:
    text = file.read()

# text = text.lower()

sents = sent_tokenize(text)

# Finding the Semester
currSent = sents[0]
currSent = currSent.lower()
words = word_tokenize(currSent)
words = [word for word in words if word not in sw]

tagged = pos_tag(words)

for word, tag in tagged:
    if tag == "VBP":
        semester = word
        break
print(semester)

# Finding the Branch
currSent = sents[1]
currSent = currSent.lower()
words = word_tokenize(currSent)
words = [word for word in words if word not in sw]

tagged = pos_tag(words)

for word, tag in tagged:
    if tag == "NN" and word != "branch":
        branch = word
        break

print(branch)

# Adding new courses
currSent = sents[2]
currSent = currSent.lower()
polarityScores = sia.polarity_scores(currSent)
if polarityScores["neg"] > polarityScores["pos"]:
    addNewCourses = "n"
else:
    addNewCourses = "y"

print(addNewCourses)


# Deleting courses
currSent = sents[3]
currSent = currSent.lower()
polarityScores = sia.polarity_scores(currSent)
if polarityScores["neg"] > polarityScores["pos"]:
    deleteCourses = "n"
else:
    deleteCourses = "y"

print(deleteCourses)


# Getting the details of minors
currSent = sents[4]
currSent = currSent.lower()
polarityScores = sia.polarity_scores(currSent)
# print(currSent, polarityScores)
if polarityScores["neg"] > polarityScores["pos"]:
    minor = "no"
else:
    words = word_tokenize(currSent)
    words = [word for word in words if word not in sw]
    tagged = pos_tag(words)
    for word, tag in tagged:
        if tag == "VBP":
            minor = word
            break


# Getting career specific courses
currSent = sents[5]
currSent = currSent.lower()
polarityScores = sia.polarity_scores(currSent)
print(currSent, polarityScores)
if polarityScores["neg"] > polarityScores["pos"]:
    career = "n"
else:
    words = word_tokenize(currSent)
    words = [word for word in words if word not in sw]
    tagged = pos_tag(words)
    for word, tag in tagged:
        if tag == "NN" and word != "career":
            career = word
            break
print(career)



# # Getting previous courses
# currSent = sents[4]
# words = word_tokenize(currSent)
# words = [word for word in words if word not in sw]
# tagged = pos_tag(words)
# prevCourses = []
# for word, tag in tagged:
#     if tag == "NNP":
#         prevCourses.append(word)
# prevCourses = formatLists(prevCourses)
# print(prevCourses)


# for token in tokens:
	# print(token, token.pos_)
	# if (token.pos_ == "PROPN"):
		# branch = token.text


swipl = Prolog() 
swipl.consult('./electiveAdvisorySystem.pl')
courses = list(swipl.query('start(%s, %s, %s, %s, %s, %s)' % (semester, branch, addNewCourses, deleteCourses, minor, career)))


# print(courses)

# print("working")

