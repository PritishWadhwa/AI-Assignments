import os

os.environ["OPENBLAS_NUM_THREADS"] = "1"
os.environ["MKL_NUM_THREADS"] = "1"

from pyswip import Prolog
from nltk.corpus import stopwords
from nltk.tokenize import word_tokenize, sent_tokenize
from nltk import pos_tag
from nltk.sentiment import SentimentIntensityAnalyzer

sia = SentimentIntensityAnalyzer()
sw = set(stopwords.words("english"))

with open("input.txt", mode="r", encoding="utf-8") as file:
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


# Getting new course domain
currSent = sents[6]
currSent = currSent.lower()
words = word_tokenize(currSent)
words = [word for word in words if word not in sw]
tagged = pos_tag(words)
for word, tag in tagged:
    if tag == "VBP" or tag == "VBZ" or tag == "DT":
        domain = word
        break

print(domain)

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
swipl.consult("./electiveAdvisorySystem.pl")
ans = list(
    swipl.query(
        "start(%s, %s, %s, %s, %s, %s, %s)"
        % (semester, branch, addNewCourses, deleteCourses, minor, career, domain)
    )
)
# ans = list(swipl.query("start"))


# print(ans)

# for i in ans:
# print(i)
# break
# break


# print(courses)

# print("working")
