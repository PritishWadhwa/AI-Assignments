from durable.lang import *

def getCSEInterest():
    interest = input(
        '''What is your interest area in computer science?: 
        1. Machine Learning
        2. Software Engineering
        3. Human Computer Interaction
        4. Robotics
Enter your choice number: ''')
    acronym = ''
    if interest == '1':
        acronym = 'ml'
        return acronym
    elif interest == '2':
        acronym = 'se'
        return acronym
    elif interest == '3':
        acronym = 'hci'
        return acronym
    elif interest == '4':
        acronym = 'rob'
        return acronym

def getECEInterest():
    interest = input(
        '''What is your interest area in electronics and communication?:
        1. Machine Learning
        2. VSLI
        3. Signal Processing
        4. Communication Systems
Enter your choice number: ''')
    acronym = ''
    if interest == '1':
        acronym = 'ml'
        return acronym
    elif interest == '2':
        acronym = 'vlsi'
        return acronym
    elif interest == '3':
        acronym = 'signal processing'
        return acronym
    elif interest == '4':
        acronym = 'comm systems'
        return acronym

def getMathInterest():
    interest = input(
        '''What is your interest area in mathematics?: 
        1. Probability
        2. Linear Algebra
        3. Calculus
        4. Statistics
Enter your choice number: ''')
    acronym = ''
    if interest == '1':
        acronym = 'probability'
        return acronym
    elif interest == '2':
        acronym = 'lin alg'
        return acronym
    elif interest == '3':
        acronym = 'calculus'
        return acronym
    elif interest == '4':
        acronym = 'statistics'
        return acronym
    
def getDesInterest():
    interest = input(
        '''What is your interest area in design?: 
        1. User Experience
        2. User Interface
        3. Visual Design
        4. Human Computer Interaction
Enter your choice number: ''')
    acronym = ''
    if interest == '1':
        acronym = 'ux'
        return acronym
    elif interest == '2':
        acronym = 'ui'
        return acronym
    elif interest == '3':
        acronym = 'visual des'
        return acronym
    elif interest == '4':
        acronym = 'hci'
        return acronym
    
def getBioInterest():
    interest = input(
        '''What is your interest area in biology?: 
        1. Genetics
        2. Microbiology
        3. Biochemistry
        4. Immunology
Enter your choice number: ''')
    acronym = ''
    if interest == '1':
        acronym = 'genetics'
        return acronym
    elif interest == '2':
        acronym = 'microbiology'
        return acronym
    elif interest == '3':
        acronym = 'biochemistry'
        return acronym
    elif interest == '4':
        acronym = 'immunology'
        return acronym

def getEcoInterest():
    interest = input(
        '''What is your interest area in economics?: 
        1. Microeconomics
        2. Macroeconomics
        3. Econometrics
        4. Finance
Enter your choice number: ''')
    acronym = ''
    if interest == '1':
        acronym = 'microeconomics'
        return acronym
    elif interest == '2':
        acronym = 'macroeconomics'
        return acronym
    elif interest == '3':
        acronym = 'econometrics'
        return acronym
    elif interest == '4':
        acronym = 'finance'
        return acronym

    


def getGrade():
    grade = input('What is your average grade in the courses(round off to nearest grade)? (A/B/C): ')
    return grade


with ruleset('domains'):

    @when_all((m.domain == 'cse'))
    def cse(c):
        interest = getCSEInterest()
        grade = getGrade()
        c.assert_fact(
            'grade',
            {
                'interest': interest, 
                'grade': grade
            }
        )

    @when_all((m.domain == 'ece'))
    def ece(c):
        interest = getECEInterest()
        grade = getGrade()
        c.assert_fact(
            'grade',
            {
                'interest': interest, 
                'grade': grade
            }
        )

    @when_all((m.domain == 'math'))
    def math(c):
        interest = getMathInterest()
        grade = getGrade()
        c.assert_fact(
            'grade',
            {
                'interest': interest, 
                'grade': grade
            }
        )

    @when_all((m.domain == 'des'))
    def des(c):
        interest = getDesInterest()
        grade = getGrade()
        c.assert_fact(
            'grade',
            {
                'interest': interest, 
                'grade': grade
            }
        )

    @when_all((m.domain == 'bio'))
    def bio(c):
        interest = getBioInterest()
        grade = getGrade()
        c.assert_fact(
            'grade',
            {
                'interest': interest, 
                'grade': grade
            }
        )

    @when_all((m.domain == 'eco'))
    def eco(c):
        interest = getEcoInterest()
        grade = getGrade()
        c.assert_fact(
            'grade',
            {
                'interest': interest, 
                'grade': grade
            }
        )

    @when_all(+m.subject)
    def output(c):
        print('Fact: {0} {1} {2}'.format(c.m.subject, c.m.predicate, c.m.object))


with ruleset('grade'):

    @when_all((m.grade == 'A'))
    def a(c):
        c.assert_fact(
            {
                'subject' : 'Do Engineering',
                'predicate' : 'in',
                'object' : c.m.interest
            }
        )

    @when_all((m.grade == 'B'))
    def b(c):
        c.assert_fact(
            {
                'subject' : 'Do Research',
                'predicate' : 'in',
                'object' : c.m.interest
            }
        )
    
    @when_all((m.grade == 'C'))
    def c(c):
        c.assert_fact(
            {
                'subject' : 'Do Apprenticeship',
                'predicate' : 'in',
                'object' : c.m.interest
            }
        )

    @when_all(+m.subject)
    def output(c):
        print('Fact: {0} {1} {2}'.format(c.m.subject, c.m.predicate, c.m.object))

courses = {
    'cse' : ['ai', 'ml', 'dl', 'nlp', 'robotics'],
    'math': ['probability', 'number theory', 'linear algebra', 'calculus', 'discrete math'],
    'ece': ['circuit theory', 'digital logic', 'analog electronics', 'signal processing', 'control systems'],
    'design': ['graphic design', 'game design', 'hci', 'wardi', 'ux'],
    'bio': ['iqb', 'bioinformatics', 'biochemistry', 'biophysics', 'biostatistics'],
    'eco': ['microeconomics', 'macroeconomics', 'game theory', 'econometrics', 'finance']
}

domains = {
    0 : 'cse',
    1 : 'math',
    2 : 'ece',
    3 : 'design',
    4 : 'bio',
    5 : 'eco'
}

def getDomain():
    domCount = []
    for i in domains:
        count = 0
        choice = input('Did you take any course of ' + domains[i] + ' domain? (y/n): ')
        if choice == 'y':
            for j in courses[domains[i]]:
                choice = input('Did you take ' + j + ' course? (y/n): ')
                if choice == 'y':
                    count += 1
        domCount.append(count)
    return domCount.index(max(domCount))

assert_fact(
    'domains', 
    { 
        'domain': domains[getDomain()]
    }
)