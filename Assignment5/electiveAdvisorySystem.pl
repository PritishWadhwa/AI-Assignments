% making sure that the user can add new courses and remove old courses from the offerings
:- dynamic(course/7).

start(SemType, Branch, Add, Delete, Minor, Career, DepName) :-
% start :-
    write('Elective Advisory System for IIITD Btech Students'), nl,
    addCourse(Add),
    deleteCourse(Delete), 
    write('Enter the courses done in the previous semester: (type done to stop)'), nl,
    take_input(CoursesDone),
    suggestCoreCourses(SemType, Branch, CoursesDone),
    suggestMinors(SemType, Minor, CoursesDone),
    suggestCareerCourses(Career, CoursesDone, SemType),
    write('Enter the courses you have already decided to do in the current semester: (type done to stop)'), nl,
    take_input(CoursesDoing),
    combineList(CoursesDone, CoursesDoing, AllCourses),
    getCourses(SemType, DepName, CoursesDone, CoursesDoing, AllCourses), !.

% If the user does not want courses related to career advice
suggestCareerCourses(n, _, _) :- !.

% suggesting courses to the user relating to his/her choice of careers
suggestCareerCourses(CareerPref, CoursesDone, SemType) :-
    % write('Enter the career preference (ml: Machine Learning/ Data Science, bd: Big Data, sec: Security Related): '), nl,
    % read(CareerPref),
    career(CareerPref, Courses),
    write('Suggested courses for your career preference: '), nl,
    printListAfterRemovingAndCheckingSemesterAndPrereqs(Courses, CoursesDone, SemType).

% Deleting the courses as required by the user
deleteCourse(n) :- !.

deleteCourse(y) :-
    write('Enter the course abbreviation to delete: '), nl,
    read(CourseAbbr),
    retract(course(_, CourseAbbr, _, _, _, _, _)),
    write('Do you want to delete more courses? (y/n): '), nl,
    read(Delete),
    deleteCourse(Delete).

% Adding new courses as defined by the user
addCourse(n) :- !.

addCourse(y) :-
    write('Enter the course code to add: '), nl,
    read(CourseCode),
    write('Enter the course abbreviation to add: '), nl,
    read(CourseAbbr),
    write('Enter the course name to add: '), nl,
    read(CourseName),
    write('Enter the pre-requisites: (done to stop)'), nl,
    take_input(PreReqs),
    remover(done, PreReqs, PreReqs1),
    write('Enter the anti-requisites: (done to stop)'), nl,
    take_input(AntiReqs),
    remover(done, AntiReqs, AntiReqs1),
    write('Enter the course department name (cse/ece/mth/bio/des/ssh/oth): '), nl,
    read(DepName),
    write('Enter the course semester type (monsoon/winter): '), nl,
    read(SemType),
    assert(course(CourseCode, CourseAbbr, CourseName, PreReqs1, AntiReqs1, [SemType], [DepName])),
    write('Are there new courses you want to add? (y/n): '), nl,
    read(Add),
    addCourse(Add).

% Function to remove all occurance of a given element from the given list
remover( _, [], []).
remover( R, [R|T], T2) :- remover( R, T, T2).
remover( R, [H|T], [H|T2]) :- H \= R, remover( R, T, T2).

% Function to combine two given list
combineList([],L,L) :- !.
combineList([H|T],L,[H|Z]) :- 
    combineList(T,L,Z).

% Taking courses input from the user and storing them in a list
take_input([Head | Tail]) :-
    write('Enter the course name: '), nl,
    read(Head),
    (Head = done -> Tail = [] ; take_input(Tail)).

% Code to print Course code, Course Abbreviation, Course Name for each course present in the list
printFullList([]) :- !.
printFullList([done]) :- !.
printFullList([Head | Tail]) :-
    course(Code, Head, Name, _, _, _, _),
    format('~w ~w ~w~n', [Code, Head, Name]),
    printFullList(Tail).

% Function to predict electives for a given department and semester type
getCourses(SemType, DepName, CoursesDone, CoursesDoing, AllCourses) :-
    findall(ShortName, course(_, ShortName, _, _, _, [SemType], [DepName]), L),
    printListAfterRemovingAndCheckingPrereqs(L, CoursesDone, CoursesDoing, AllCourses).

% code to print only those elements which are present in list 1 but not in list 2
printListAfterRemoving([], _) :- !.
printListAfterRemoving([ShortName | Tail], PastCourses) :-
    \+ member(ShortName, PastCourses),
    course(Code, ShortName, FullName, _, _, _, _),
    format('~w ~w ~w~n', [Code, ShortName, FullName]),
    printListAfterRemoving(Tail, PastCourses);
    printListAfterRemoving(Tail, PastCourses).

% code to print the courses list after checking the course prereqs
printListAfterRemovingAndCheckingPrereqs([], _, _, _) :- !.

printListAfterRemovingAndCheckingPrereqs([ShortName | Tail], CoursesDone, CoursesDoing, AllCourses) :-
    \+ member(ShortName, AllCourses),
    course(Code, ShortName, FullName, Prereqs, Antireqs, _, _),
    foreach(member(Prereq, Prereqs), checkPrereq(Prereq, CoursesDone)),
    foreach(member(Antireq, Antireqs), \+ checkAntireq(Antireq, AllCourses)),
    format('~w ~w ~w~n', [Code, ShortName, FullName]),
    printListAfterRemovingAndCheckingPrereqs(Tail, CoursesDone, CoursesDoing, AllCourses);
    printListAfterRemovingAndCheckingPrereqs(Tail, CoursesDone, CoursesDoing, AllCourses).

% code to suggest the core courses
suggestCoreCourses(SemType, Branch, PastCourses) :-
    core(Branch, CoreCourses),
    write('Core Courses to do are: '), nl,
    printListAfterRemovingAndCheckingSemesterAndPrereqs(CoreCourses, PastCourses, SemType).

% code to suggest the minors
suggestMinors(_, no, _) :- !.

suggestMinors(SemType, Minor, PastCourses) :-
    Minor \= no,
    minors(Minor, MinorCourses),
    write('Minor Courses to do are: '), nl,
    printListAfterRemovingAndCheckingSemesterAndPrereqs(MinorCourses, PastCourses, SemType).

% code to suggest the courses after checking for antireqs
printListAfterRemovingAndCheckingSemesterAndPrereqs([], _, _) :- 
    write('Congratulations, No More Core Courses to do!') , nl, !.

printListAfterRemovingAndCheckingSemesterAndPrereqs([ShortName | Tail], PastCourses, SemType) :-
    \+ member(ShortName, PastCourses),
    course(Code, ShortName, FullName, Prereqs, _, [SemType], _),
    foreach(member(Prereq, Prereqs), checkPrereq(Prereq, PastCourses)),
    format('~w ~w ~w~n', [Code, ShortName, FullName]),
    printListAfterRemovingAndCheckingSemesterAndPrereqs(Tail, PastCourses, SemType);
    printListAfterRemovingAndCheckingSemesterAndPrereqs(Tail, PastCourses, SemType).

% code to print the courses list after checking the semester the course is offered in
printListAfterRemovingAndCheckingSemester([], _, _) :- 
    write('Congratulations, No More Core Courses to do!') , nl, !.

printListAfterRemovingAndCheckingSemester([ShortName | Tail], PastCourses, Semester) :-
    \+ member(ShortName, PastCourses),
    course(Code, ShortName, FullName, _, _, [Semester], _),
    format('~w ~w ~w~n', [Code, ShortName, FullName]),
    printListAfterRemovingAndCheckingSemester(Tail, PastCourses, Semester);
    printListAfterRemovingAndCheckingSemester(Tail, PastCourses, Semester).

% code to check if the prereq of a given course is done or not
checkPrereq(Prereq, PastCourses) :-
    course(Prereq, Code, _, _, _, _, _),
    member(Code, PastCourses), !;
    fail.

% code to check if the antireq of a given course is done or not
checkAntireq(Antireq, PastCourses) :-
    course(Antireq, Code, _, _, _, _, _),
    member(Code, PastCourses), !;
    fail.

% Core Courses
core(cse, ['IP', 'DC', 'M-I', 'PIS', 'COM', 'DSA', 'BE', 'P&S', 'CO', 'AP', 'OS', 'DM', 'DBMS', 'ADA', 'CN', 'EEE', 'TCOM']).
core(ece, ['IP', 'DC', 'M-I', 'PIS', 'COM', 'DSA', 'BE', 'P&S', 'CO', 'CTD', 'ELD', 'S&S', 'M-III', 'F&W', 'IE', 'PCS', 'M-IV', 'EEE', 'TCOM']).
core(csam, ['IP', 'DC', 'M-I', 'PIS', 'COM', 'DSA', 'BE', 'P&S', 'CO', 'RA-I', 'OS', 'DM', 'M-IV', 'ADA', 'AA-I', 'EEE', 'TOC', 'SPA', 'SI', 'TCOM']).
core(csd, ['IP', 'DC', 'M-I', 'PIS', 'COM', 'DSA', 'DDV', 'P&S', 'CO', 'VDC', 'AP', 'OS', 'DPP', 'RMSSD', 'DBMS', 'ADA', 'DIS', 'CN', 'EEE', 'TCOM']).
core(csss, ['IP', 'DC', 'M-I', 'PIS', 'COM', 'DSA', 'ISA', 'P&S', 'CO', 'CTRSS', 'OS', 'RMSSD', 'DM', 'AP', 'M-III', 'ADA', 'CO', 'DBMS', 'ECO', 'EEE', 'TCOM']).
core(csb, ['IP', 'DC', 'M-I', 'PIS', 'COM', 'DSA', 'BE', 'P&S', 'CO', 'FOB', 'AP', 'OS', 'M-III', 'CBBC', 'GMB', 'DBMS', 'ALD', 'CN', 'PB', 'IQB', 'ABIN', 'ACB', 'EEE', 'TCOM']).
core(csai, ['IP', 'DC', 'M-I', 'PIS', 'COM', 'DSA', 'IIS', 'P&S', 'AP', 'OS', 'M-III', 'DM', 'S&S', 'SML', 'ADA', 'ML', 'AI', 'EEE', 'TCOM', 'EI']).

% Mandatory courses to be done for minors
minors(eco, ['ME', 'GMT', 'ECO', 'P&S']).
minors(bio, ['IQB']).
minors(ent, ['EK', 'NVP', 'EComm']).

% Courses to be done for specific career paths
career(ml, ['P&S', 'ML', 'M-I', 'M-III', 'CV', 'SML', 'NLP', 'DL', 'AI', 'DMG', 'CF', 'AML']).
career(db, ['DBMS', 'IIA', 'SWeb', 'IR', 'DMG', 'DBSI', 'ISC', 'BDA', 'DW']).
career(sec, ['CN', 'DSCD', 'SDN', 'ATMC', 'NSS-II', 'PN', 'WN', 'OS', 'FCS', 'TAC', 'SE', 'MS', 'PSOSM', 'NSC']).


% CSE Courses
course('CSE566/DES5MMS', 'MMS', 'Mobile and Middleware Systems', ['CSE535'], [], [monsoon], [cse]).
course('BIO546/CSE585', 'CM', 'Computing for Medicine', [], [], [monsoon], [cse]).
course('CSE344/CSE544/ECE344/ECE544', 'CV', 'Computer Vision', ['MTH100'], [], [winter], [cse]).
course('CSE340/CSE540/ECE340', 'DIP', 'Digital Image Processing', ['MTH100', 'MTH201'], [], [monsoon], [cse]).
course('CSE320/CSE520', 'AA', 'Advanced Algorithms', ['CSE102'], [], [monsoon], [cse]).
course('CSE562', 'ACV', 'Advanced Computer Vision', ['CSE344/CSE544/ECE344/ECE544'], [], [monsoon], [cse]).
course('CSE642', 'AML', 'Advanced Machine Learning', ['MTH201', 'CSE343/CSE543/ECE563', 'CSE342/CSE542'], [], [monsoon], [cse]).
course('CSE201', 'AP', 'Advanced Programming', ['CSE101, CSE102'], [], [monsoon], [cse]).
course('CSE661/DES507', 'AFC', 'Affective Computing', ['CSE101', 'CSE102', 'CSE201'], [], [monsoon], [cse]).
course('CSE643', 'AI', 'Artificial Intelligence', ['CSE102'], [], [monsoon], [cse]).
course('CSE515/ECE551', 'BML', 'Bayesian Machine Learning', ['MTH201'], [], [monsoon], [cse]).
course('CSE569', 'CldC', 'Cloud Computing', ['CSE232'], [], [monsoon], [cse]).
course('CSE640', 'CF', 'Collaborative Filtering', ['MTH100'], [], [monsoon], [cse]).
course('CSE301/CSE601', 'CMP', 'Compilers', ['MTH100', 'CSE101', 'CSE201', 'CSE102'], [], [monsoon], [cse]).
course('CSE421/CSE621', 'CMPT', 'Complexity Theory', ['CSE102', 'MTH201', 'CSE322', 'CSE121'], [], [monsoon], [cse]).
course('CSE511/ECE511', 'CA', 'Computer Architecture', [], [], [monsoon], [cse]).
course('CSE333/CSE533', 'CG', 'Computer Graphics', ['CSE101'], [], [monsoon], [cse]).
course('CSE232', 'CN', 'Computer Networks', ['CSE101', 'CSE231', 'CSE222'], [], [monsoon], [cse]).
course('CSE606A', 'DML', 'Data Lifecycle Management', ['CSE343/CSE543/ECE563'], [], [monsoon], [cse]).
course('CSE506', 'DMG', 'Data Mining', ['CSE202', 'CSE101', 'MTH100', 'MTH201'], [], [monsoon], [cse]).
course('CSE558', 'DSc', 'Data Science', ['CSE101'], [], [monsoon], [cse]).
course('CSE507', 'DBSI', 'Database System Implementation', ['CSE202', 'CSE102'], [], [monsoon], [cse]).
course('CSE501', 'DHCS', 'Designing Human Centered Systems', [], ['DES204'], [monsoon], [cse]).
course('CSE121', 'DM', 'Discrete Mathematics', [], ['MTH210'], [monsoon], [cse]).
course('CSE530', 'DSCD', 'Distributed Systems: Concepts & Design', ['CSE232', 'CSE222'], [], [monsoon], [cse]).
course('CSE345/CSE545', 'FCS', 'Foundations of Computer Security', [], [], [monsoon], [cse]).
course('CSE656', 'IIA', 'Information Integration and Applications', ['CSE202'], [], [monsoon], [cse]).
course('CSE525', 'GA', 'Introduction to Graduate Algorithms', ['CSE222'], [], [monsoon], [cse]).
course('CSE101', 'IP', 'Introduction to Programming', [], [], [monsoon], [cse]).
course('CSE555', 'ISC', 'Introduction to Spatial Computing', ['CSE102', 'CSE202'], [], [monsoon], [cse]).
course('CSE605A', 'KG', 'Knowledge Graphs in Practice', ['CSE508'], [], [monsoon], [cse]).
course('CSE535', 'MC', 'Mobile Computing', ['CSE101'], [], [monsoon], [cse]).
course('CSE319/CSE519', 'MAD', 'Modern Algorithm Design', ['CSE222'], [], [monsoon], [cse]).
course('CSE556', 'NLP', 'Natural Language Processing', ['CSE101', 'MTH201', 'CSE222', 'MTH100'], [], [monsoon], [cse]).
course('CSE749', 'NAP', 'Network Anonymity and Privacy', [], [], [monsoon], [cse]).
course('CSE350/CSE550', 'NSC', 'Network Security', [], [], [monsoon], [cse]).
course('CSE231', 'OS', 'Operating Systems', ['CSE102'], [], [monsoon], [cse]).
course('CSE513', 'PRMP', 'Parallel Runtimes for Modern Processors', ['CSE201', 'CSE231'], [], [monsoon], [cse]).
course('CSE503', 'PA', 'Program Analysis', ['CSE102', 'CSE201'], [], [monsoon], [cse]).
course('CSE584', 'PV', 'Program Verification', ['CSE121'], [], [monsoon], [cse]).
course('CSE523', 'RA', 'Randomized Algorithms', ['CSE222', 'CSE121', 'MTH201'], [], [monsoon], [cse]).
course('CSE564', 'RL', 'Reinforcement Learning', ['MTH201'], [], [monsoon], [cse]).
course('CSE576', 'SSIoT', 'Smart Sensing for Internet of Things (IoT)', [], [], [monsoon], [cse]).
course('CSE565', 'SDN', 'Software Defined Networking', ['CSE232'], [], [monsoon], [cse]).
course('CSE583', 'SDOS', 'Software Development using Open Source', [], [], [monsoon], [cse]).
course('CSE582', 'SPEM', 'Software Production Evolution and Maintenance', [], [], [monsoon], [cse]).
course('CSE572/ECE561', 'SAP', 'Speech and Audio Processing', ['MTH100', 'ECE250'], [], [monsoon], [cse]).
course('CSE524', 'TMC', 'Theory of Modern cryptography', [], [], [monsoon], [cse]).
course('CSE651', 'TAC', 'Topics in Adaptive Cybersecurity', ['CSE345/CSE545'], [], [monsoon], [cse]).
course('CSE701', 'TSE', 'Topics in Software Engineering: AI in SE', [], [], [monsoon], [cse]).
course('CSE546', 'AC', 'Applied Cryptography', ['CSE121'], [], [monsoon, winter], [cse]).
course('CSE5GP', 'GP', 'Geometry Processing', ['CSE101'], [], [monsoon, winter], [cse]).
course('CSE571', 'IMC', 'Introduction to Media Computing', [], [], [monsoon, winter], [cse]).
course('CSE343/CSE543/ECE563', 'ML', 'Machine Learning', ['MTH100', 'MTH201', 'CSE101', 'MTH203'], [], [monsoon], [cse]).
course('CSE343/CSE543/ECE563', 'ML', 'Machine Learning', ['MTH100', 'MTH201', 'CSE101', 'MTH203'], [], [winter], [cse]).
course('CSE531', 'MAS', 'Multi-Agent Systems', [], [], [monsoon], [cse]).
course('CSE531', 'MAS', 'Multi-Agent Systems', [], [], [winter], [cse]).
course('CSE580A', 'OM', 'Ontology Modeling', ['CSE102'], [], [monsoon], [cse]).
course('CSE580A', 'OM', 'Ontology Modeling', ['CSE102'], [], [winter], [cse]).
course('CSE552', 'SE', 'Security Engineering', ['CSE231'], [], [monsoon], [cse]).
course('CSE552', 'SE', 'Security Engineering', ['CSE231'], [], [winter], [cse]).
course('CSE441/CSE541', 'BIOM', 'Advanced Biometrics', ['CSE343/CSE543/ECE563'], [], [winter], [cse]).
course('CSE577', 'AIOT', 'Advanced Internet of Things', [], [], [winter], [cse]).
course('CSE631', 'AOS', 'Advanced Operating Systems', ['CSE231'], [], [winter], [cse]).
course('CSE634', 'ATMC', 'Advanced Topics in Mobile Computing', ['CSE232'], [], [winter], [cse]).
course('CSE222', 'ADA', 'Algorithm Design and Analysis', ['CSE102'], ['CSE223'], [winter], [cse]).
course('CSE223', 'ALD', 'Algorithm Design and Analysis', ['CSE102'], ['CSE222'], [winter], [cse]).
course('CSE529', 'AAG', 'Approximation Algorithms', ['CSE222'], [], [winter], [cse]).
course('CSE557', 'BDA', 'Big Data Analytics	', ['CSE202'], [], [winter], [cse]).
course('CSE636/ECE636', 'COMN', 'Communication Networks', ['MTH201'], [], [winter], [cse]).
course('CSE112', 'CO', 'Computer Organization', ['ECE111'], [], [winter], [cse]).
course('CSE518', 'CLDS', 'Concurrent and Learned Data Structures', ['CSE101', 'CSE102', 'CSE201', 'CSE231'], [], [winter], [cse]).
course('CSE102', 'DSA', 'Data Structures & Algorithms', ['CSE101'], [], [winter], [cse]).
course('CSE606', 'DW', 'Data Warehouse', ['CSE202'], [], [winter], [cse]).
course('CSE504', 'DP', 'Decision Procedures', ['CSE102', 'CSE121'], [], [winter], [cse]).
course('CSE641/ESE553', 'DL', 'Deep Learning', [], [], [winter], [cse]).
course('CSE663', 'EAI', 'Edge AI', ['CSE343/CSE543/ECE563', 'CSE342/CSE542'], [], [winter], [cse]).
course('CSE502', 'FPP', 'Foundations of Parallel Programming', ['CSE101', 'CSE102', 'CSE201'], [], [winter], [cse]).
course('CSE202', 'DBMS', 'Fundamentals of Database Management System', ['CSE102'], [], [winter], [cse]).
course('CSE560', 'GPU', 'GPU Computing', ['CSE101'], [], [winter], [cse]).
course('CSE508', 'IR', 'Information Retrieval', ['CSE102', 'CSE201', 'CSE202'], [], [winter], [cse]).
course('CSE528', 'IBC', 'Introduction to Blockchain and Cryptocurrency', ['CSE546'], [], [winter], [cse]).
course('CSE140', 'IIS', 'Introduction to Intelligent Systems', [], [], [winter], [cse]).
course('CSE622', 'IQC', 'Introduction to Quantum Computing', ['MTH100'], [], [winter], [cse]).
course('CSE577', 'MLRC', 'Machine Learning Techniques for Real-time Control', [], [], [winter], [cse]).
course('CSE618', 'MTL', 'Meta-Learning', ['CSE641'], [], [winter], [cse]).
course('CSE559', 'MLN', 'Mining Large Networks', ['CSE222', 'CSE101', 'MTH201'], [], [winter], [cse]).
course('CSE563', 'MCA', 'Multimedia Computing and Applications', [], [], [winter], [cse]).
course('CSE694F/ECE651', 'MS', 'Multimedia Security', ['ESE350'], [], [winter], [cse]).
course('CSE655', 'NS', 'Network Science', [], [], [winter], [cse]).
course('CSE354/CSE554', 'NSS-II', 'Networks and System Security II', ['CSE231', 'CSE232'], [], [winter], [cse]).
course('CSE645', 'PSOSM', 'Privacy and Security in Online Social Media', [], [], [winter], [cse]).
course('CSE651', 'PGM', 'Probabilistic Graphical Models', ['MTH201'], [], [winter], [cse]).
course('CSE567', 'PN', 'Programmable Networking', ['CSE102', 'CSE231', 'CSE232'], [], [winter], [cse]).
course('CSE633/ECE670', 'IRob', 'Robotics', [], [], [winter], [cse]).
course('CSE632', 'Sweb', 'Semantic Web', ['CSE101', 'CSE202'], [], [winter], [cse]).
course('CSE568', 'SNA', 'Social Network Analysis', ['CSE222', 'CSE101', 'MTH201'], ['CSE559'], [winter], [cse]).
course('CSE633A', 'SR', 'Social Robotics', [], [], [winter], [cse]).
course('CSE662A', 'SCV', 'Special Topics in Computer Vision', ['ECE350'], [], [winter], [cse]).
course('CSE609', 'SComp', 'Statistical Computation', ['MTH201'], [], [winter], [cse]).
course('CSE342/CSE542/ECE356/ECE556', 'SML', 'Statistical Machine Learning', ['CSE101', 'MTH201'], [], [winter], [cse]).
course('CSE581', 'SADRE', 'Systems Analysis, Design and Requirements Engineering', [], [], [winter], [cse]).
course('CSE516/ECE559', 'TDL', 'Theories of Deep Learning', ['MTH100', 'MTH203', 'CSE343/CSE543/ECE563'], [], [winter], [cse]).
course('CSE322', 'ToC', 'Theory of Computation', ['CSE121'], [], [winter], [cse]).
course('CSE793A', 'TC', 'Topics in Cryptanalysis', [], [], [winter], [cse]).
course('CSE660', 'TAIS', 'Trustworthy AI systems', ['CSE643', 'CSE343/CSE543/ECE563'], ['CSE660A'], [winter], [cse]).
course('CSE570', 'VR', 'Virtual Reality', [], [], [winter], [cse]).
course('CSE538/ECE538', 'WN', 'Wireless Networks', ['CSE232'], [], [winter], [cse]).

% ECE Courses
course('CSE538/ECE538', 'WN', 'Wireless Networks', ['CSE232'], [], [winter], [ece]).
course('CSE516/ECE559', 'TDL', 'Theories of Deep Learning', ['MTH100', 'MTH203', 'CSE343/CSE543/ECE563'], [], [winter], [ece]).
course('CSE342/CSE542/ECE356/ECE556', 'SML', 'Statistical Machine Learning', ['CSE101', 'MTH201'], [], [winter], [ece]).
course('CSE694F/ECE651', 'MS', 'Multimedia Security', ['ESE350'], [], [winter], [ece]).
course('CSE641/ESE553', 'DL', 'Deep Learning', [], [], [winter], [ece]).
course('CSE636/ECE636', 'COMN', 'Communication Networks', ['MTH201'], [], [winter], [ece]).
course('CSE343/CSE543/ECE563', 'ML', 'Machine Learning', ['MTH100', 'MTH201', 'CSE101', 'MTH203'], [], [monsoon], [ece]).
course('CSE343/CSE543/ECE563', 'ML', 'Machine Learning', ['MTH100', 'MTH201', 'CSE101', 'MTH203'], [], [winter], [ece]).
course('CSE572/ECE561', 'SAP', 'Speech and Audio Processing', ['MTH100', 'ECE250'], [], [monsoon], [ece]).
course('CSE511/ECE511', 'CA', 'Computer Architecture', [], [], [monsoon], [ece]).
course('CSE515/ECE551', 'BML', 'Bayesian Machine Learning', ['MTH201'], [], [monsoon], [ece]).
course('ECE315/ECE515', 'CMOS', 'Analog CMOS Circuit Design', [], [], [monsoon], [ece]).
course('ECE431/ECE631', 'ATD', 'Antennas Theory and Design', ['ECE230'], [], [monsoon], [ece]).
course('ECE215', 'CTD', 'Circuit Theory and Devices', [], [], [monsoon], [ece]).
course('ECE554', 'CS', 'Compressive Sensing', ['MTH100'], [], [monsoon], [ece]).
course('ECE560', 'DPM', 'Data Processing and Management', [], ['CSE201'], [monsoon], [ece]).
course('ECE111', 'DC', 'Digital Circuits', [], [], [monsoon], [ece]).
course('ECE340', 'DCS', 'Digital Communication Systems', ['ECE240'], [], [monsoon], [ece]).
course('ECE510', 'DHD', 'Digital Hardware Design', ['ECE270'], [], [monsoon], [ece]).
course('CSE340/CSE540/ECE340', 'DIP', 'Digital Image Processing', ['MTH100', 'MTH201'], [], [monsoon], [ece]).
course('ECE351', 'DSP', 'Digital Signal Processing', ['ECE250'], [], [monsoon], [ece]).
course('ECE314/ECE514', 'DVD', 'Digital VLSI Design', [], [], [monsoon], [ece]).
course('ECE270', 'ELD', 'Embedded Logic Design', [], [], [monsoon], [ece]).
course('ECE522', 'ICF', 'Integrated Circuit Fabrication', [], [], [monsoon], [ece]).
course('ECE519', 'IAI', 'Intelligent Applications Implementation on Heterogeneous Platforms', ['CSE112'], [], [monsoon], [ece]).
course('ECE570', 'LST', 'Linear Systems Theory', [], [], [monsoon], [ece]).
course('ECE520', 'LVACD', 'Low Voltage Analog Circuit Design', ['ECE315/ECE515'], [], [monsoon], [ece]).
course('ECE579', 'NLR', 'Nonlinear and Adaptive Control of Robotic Systems', [], [], [monsoon], [ece]).
course('ECE546', 'OWC', 'Optical and Wireless Convergence for Beyond 5G Networks and IoT', [], [], [monsoon], [ece]).
course('ECE532', 'OFNI', 'Optical Fiber Networks for Industry 4.0', [], [], [monsoon], [ece]).
course('ECE545', 'Pho', 'Photonics: Fundamentals & Applications', ['ECE230'], [], [monsoon], [ece]).
course('ECE543', 'PDCS', 'Principals of Digital Communication System', ['ECE240'], [], [monsoon], [ece]).
course('ECE501', 'PRP', 'Probability and Random Process', ['ECE240'], [], [monsoon], [ece]).
course('ECE524', 'QMD', 'Quantum Materials and Devices', [], [], [monsoon], [ece]).
course('ECE525', 'QM', 'Quantum Mechanics', ['MTH100'], [], [monsoon], [ece]).
course('ECE533', 'SNSF', 'Satellite Navigation and Sensor Fusion', ['ECE351'], [], [monsoon], [ece]).
course('ECE250', 'S&S', 'Signals and Systems', ['MTH100'], [], [monsoon], [ece]).
course('ECE318', 'SSD', 'Solid State Devices', [], [], [monsoon], [ece]).
course('ECE516', 'SCDT', 'System on Chip Design and Test', ['ECE314/ECE514', 'CSE234'], [], [monsoon], [ece]).
course('ECE565', 'TI', 'Tomographic Imaging', ['MTH100'], [], [monsoon], [ece]).
course('ECE513', 'VDF', 'VLSI Design Flow', ['ECE101'], [], [monsoon], [ece]).
course('ECE573', 'AELD', 'Advanced Embedded Logic Design', ['CSE234'], [], [winter], [ece]).
course('ECE5aom', 'AOMML', 'Applied Optimization Methods for Machine Learning', ['MTH201', 'MTH270'], [], [winter], [ece]).
course('ECE581', 'AD', 'Autonomous Driving', [], [], [winter], [ece]).
course('ECE113', 'BE', 'Basic Electronics', [], [], [winter], [ece]).
course('CSE344/CSE544/ECE344/ECE544', 'CV', 'Computer Vision', ['MTH100'], [], [winter], [ece]).
course('ECE5et', 'ET', 'Estimation Theory for dynamic systems', ['ECE501'], [], [winter], [ece]).
course('ECE230', 'F&W', 'Fields and Waves', ['MTH203'], [], [winter], [ece]).
course('ECE214', 'IE', 'Integrated Electronics', ['ECE111', 'ECE113', 'DES130'], [], [winter], [ece]).
course('ECE517', 'INE', 'Introduction to Nanoelectronics', [], [], [winter], [ece]).
course('ECE577', 'MLRC', 'Machine Learning Techniques for Real-time Control', [], [], [winter], [ece]).
course('ECE611', 'MDT', 'Memory Design and Testing', ['ECE111', 'ECE113'], [], [winter], [ece]).
course('ECE412/ECE612', 'MSD', 'Mixed Signal Design', ['ECE315/ECE515'], [], [winter], [ece]).
course('ECE343', 'MCOM', 'Mobile Communications', ['ECE240'], [], [winter], [ece]).
course('ECE55G', '5GN', 'Modeling and Analysis of Random 5G Networks', ['MTH201'], [], [winter], [ece]).
course('ECE534', 'OCS', 'Optical Communications Systems', ['ECE240'], [], [winter], [ece]).
course('ECE571', 'OCNS', 'Optimal Control Systems', [], [], [winter], [ece]).
course('ECE240', 'PCS', 'Principles of Communication Systems', ['ECE250', 'MTH201'], [], [winter], [ece]).
course('ECE432/ECE632', 'RS', 'Radar Systems', ['ECE250'], [], [winter], [ece]).
course('ECE321/ECE521', 'RFCD', 'RF Circuit Design', ['ECE230'], [], [winter], [ece]).
course('CSE633/ECE670', 'IRob', 'Robotics', [], [], [winter], [ece, cse]).
course('ECE557', 'SRU', 'Speech Recognition and Understanding', ['ECE250'], [], [winter], [ece]).
course('ECE452/ECE552', 'SSP', 'Statistical Signal Processing', ['MTH100', 'MTH201', 'ECE250'], [], [winter], [ece]).
course('ECE672', 'SEC', 'Stochastic Estimation and Control', ['ECE501'], [], [winter], [ece]).
course('ECE558', 'TLA', 'Transform Learning and Applications', ['MTH100'], ['ECE362/ECE562'], [winter], [ece]).
course('ECE537', 'WCE', 'Wireless Communication Evolution from 3G to 5G', ['ECE240'], [], [winter], [ece]).
course('ECE539', 'WSI', 'Wireless System Implementation', ['ECE240'], [], [winter], [ece]).

% Math Courses
course('MTH302', 'ALG', 'Algebra', [], ['MTH212'], [monsoon], [mth]).
course('MTH512', 'ANT', 'Algebraic Number Theory', [], [], [monsoon], [mth]).
course('MTH576', 'CDA', 'Categorical Data Analysis', ['MTH201'], [], [monsoon], [mth]).
course('MTH341/MTH541', 'CmpA', 'Complex Analysis', ['MTH240'], [], [monsoon], [mth]).
course('MTH210', 'DS', 'Discrete Structures', [], ['CSE121'], [monsoon], [mth]).
course('MTH545', 'FSEM', 'Finite and Spectral Element Methods', ['MTH100', 'MTH203/MTH240', 'MTH204'], [], [monsoon], [mth]).
course('MTH375/MTH575', 'FM', 'Fluid Mechainics', ['MTH204'], [], [monsoon], [mth]).
course('MTH564A', 'IRG', 'Introduction to Riemannian Geometry with Some Applications', ['MTH100', 'MTH203'], [], [monsoon], [mth]).
course('MTH100', 'M-I', 'Linear Algebra', [], [], [monsoon], [mth]).
course('MTH203', 'M-III', 'Multivariate Calculus', [], ['MTH240'], [monsoon], [mth]).
course('MTH211', 'NT', 'Number Theory', [], [], [monsoon], [mth]).
course('MTH562', 'PST', 'Point Set Toplogy', [], [], [monsoon], [mth]).
course('MTH562A', 'PST-2', 'Point Set Toplogy', [], ['MTH562'], [monsoon], [mth]).
course('MTH240', 'RA-I', 'Real Analysis I', [], ['MTH203'], [monsoon], [mth]).
course('MTH340', 'RA-II', 'Real Analysis-II', ['MTH240'], [], [monsoon], [mth]).
course('MTH373', 'SC', 'Scientific Computing', ['MTH100'], ['MTH270'], [monsoon], [mth]).
course('MTH371', 'SPA', 'Stochastic Processes and Applications', ['MTH201'], [], [monsoon], [mth]).
course('MTH518', 'TNT', 'Topics in Number Theory', ['MTH211'], [], [monsoon], [mth]).
course('MTH270', 'NM', 'Numerical Methods', ['MTH100', 'MTH204'], [], [monsoon], [mth]).
course('MTH270', 'NM', 'Numerical Methods', ['MTH100', 'MTH204'], [], [winter], [mth]).
course('MTH212', 'AA-I', 'Abstract Algebra I', [], ['MTH302'], [winter], [mth]).
course('MTH513', 'AA-II', 'Abstract Algebra II', ['MTH100', 'MTH212'], [], [winter], [mth]).
course('MTH510', 'ALA', 'Advanced Linear Algebra', ['MTH100'], [], [winter], [mth]).
course('MTH516', 'ANNT', 'Analytic Number Theory', ['MTH341'], [], [winter], [mth]).
course('MTH544', 'CRN', 'Calculus in R^N', ['MTH100', 'MTH203'], [], [winter], [mth]).
course('MTH514', 'COT', 'Coding Theory', ['MTH100', 'MTH212', 'MTH513'], [], [winter], [mth]).
course('MTH517', 'CMO', 'Combinatorial Optimization', ['MTH374/MTH574'], [], [winter], [mth]).
course('MTH311', 'CIA', 'Combinatorics and its Applications', ['MTH100'], [], [winter], [mth]).
course('MTH577', 'COO', 'Convex Optimization', ['MTH100'], [], [winter], [mth]).
course('MTH204', 'M-IV', 'Differential Equations', ['MTH203'], [], [winter], [mth]).
course('MTH310', 'GT', 'Graph Theory', [], [], [winter], [mth]).
course('MTH571', 'ITA', 'Integral Transforms and their Applications', ['MTH204'], [], [winter], [mth]).
course('MTH343/MTH543', 'IDS', 'Introduction to Dynamical Systems', ['MTH100', 'MTH203', 'MTH240'], [], [winter], [mth]).
course('MTH542', 'IFA', 'Introduction to Functional Analysis', ['MTH100'], [], [winter], [mth]).
course('MTH300', 'IML', 'Introduction to Mathematical Logic', [], [], [winter], [mth]).
course('MTH550', 'IPDE', 'Introduction to PDE', ['MTH204'], [], [winter], [mth]).
course('MTH374/MTH574', 'LO', 'Linear Optimization', ['MTH100'], ['MTH305/MTH505'], [winter], [mth]).
course('MTH535', 'MPT', 'Measure and Probability Theory', ['MTH240'], [], [winter], [mth]).
course('MTH598', 'NPDE', 'Numerical Partial Differential Equations', ['MTH100', 'MTH203', 'MTH240'], ['MTH570'], [winter], [mth]).
course('MTH570', 'NSDE', 'Numerical Solutions Differential Equations', ['MTH100', 'MTH203', 'MTH240'], [], [winter], [mth]).
course('MTH201', 'P&S', 'Probability and Statistics', [], [], [winter], [mth]).
course('MTH372', 'SI', 'Statistical Inference', ['MTH201'], ['CSE609'], [winter], [mth]).
course('MTH599', 'VCA', 'Variational Calculus and their Applications', ['MTH203'], [], [winter], [mth]).

% BIO Courses
course('BIO321', 'ABIN', 'Algorithms in Bioinformatics', ['CSE222'], [], [monsoon], [bio]).
course('BIO522', 'ACB', 'Algorithms in Computational Biology', [], [], [monsoon], [bio]).
course('BIO524', 'BIP', 'Biomedical Image processing', [], [], [monsoon], [bio]).
course('BIO361', 'BioP', 'Biophysics', [], [], [monsoon], [bio]).
course('BIO211', 'CBBC', 'Cell Biology and Bio-Chemistry', [], ['BIO511'], [monsoon], [bio]).
course('BIO511', 'CBB', 'Cell Biology and Bio-Chemistry', [], ['BIO211'], [monsoon], [bio]).
course('BIO544', 'CGAS', 'Computational Gastronomy', [], [], [monsoon], [bio]).
course('BIO546/CSE585', 'CM', 'Computing for Medicine', [], [], [monsoon], [bio]).
course('BIO512', 'FOMB', 'Foundations of Modern Biology', [], ['BIO601', 'BIO214'], [monsoon], [bio]).
course('BIO214', 'GMB', 'Genetics and Molecular Biology', [], [], [monsoon], [bio]).
course('ENT421/BIO571', 'HIEE', 'Healthcare Innovation and Entrepreneurship Essentials', [], [], [monsoon], [bio]).
course('BIO542', 'MLBA', 'Machine Learning for Biomedical Applications', [], [], [monsoon], [bio]).
course('BIO532', 'NB', 'Network Biology', [], [], [monsoon], [bio]).
course('BIO506', 'SSSBB', 'Stochastic Simulations in Systems Biology and Biophysics', ['BIO531'], [], [monsoon], [bio]).
course('BIO543', 'BDMH', 'Big Data Mining in Healthcare', [], [], [winter], [bio]).
course('BIO545', 'BioStats', 'Biostatistics', [], [], [winter], [bio]).
course('BIO523', 'CHI', 'Chemoinformatics', [], [], [winter], [bio]).
course('BIO561', 'CADD', 'Computer Aided Drug Design', [], [], [winter], [bio]).
course('BIO541', 'DSG', 'Data Sciences for Genomics', [], [], [winter], [bio]).
course('BIO101', 'FOB', 'Foundations of Biology', [], [], [winter], [bio]).
course('BIO534', 'ICN', 'Introduction to Computational Neuroscience', [], ['BIO505'], [winter], [bio]).
course('BIO531', 'IMB', 'Introduction to Mathematical Biology', ['MTH100'], ['BIO303/BIO503'], [winter], [bio]).
course('BIO213', 'IQB', 'Introduction to Quantitative Biology', ['MTH100'], [], [winter], [bio]).
course('BIO221', 'PB', 'Practical Bioinformatics', [], [], [winter], [bio]).
course('BIO533', 'S&SB', 'Systems and Synthetic Biology', ['BIO531', 'BIO501'], [], [winter], [bio]).

% DES courses
course('CSE566/DES5MMS', 'MMS', 'Mobile and Middleware Systems', ['CSE535'], [], [monsoon], [des]).
course('DES305', '3DAF', '3D Animation filmmaking', [], [], [monsoon], [des]).
course('CSE661/DES507', 'AFC', 'Affective Computing', ['CSE101', 'CSE102', 'CSE201'], [], [monsoon], [des]).
course('DES509', 'DF', 'Design Futures', ['DES205/DES519'], [], [monsoon], [des]).
course('DES205/DES519', 'DIS', 'Design of Interactive Systems', [], [], [monsoon], [des]).
course('DES201', 'DPP', 'Design Processes and Perspectives', ['DES101'], [], [monsoon], [des]).
course('DES514', 'DAVP', 'Digital Audio & Video Production Workflow', [], [], [monsoon], [des]).
course('DES524', 'EFD', 'Ergonomics/Human factors for Design', [], [], [monsoon], [des]).
course('DES303', 'FMRP', 'Film Making and Radio Podcasting', [], [], [monsoon], [des]).
course('DES501', 'FAE', 'Fundamentals of Audio for Engineers', [], [], [monsoon], [des]).
course('DES520', 'HCAI', 'Human Centred AI', ['DES204', 'CSE343/CSE543/ECE563'], [], [monsoon], [des]).
course('DES522', 'IDUDA', 'Inclusive Design, Universal Design & Accessibility', ['DES201'], [], [monsoon], [des]).
course('DES515', 'ISPR', 'Information Systems in Public Health', [], [], [monsoon], [des]).
course('DES516', 'I3DD', 'Introduction to 3D Production Design for Animation and Games', [], [], [monsoon], [des]).
course('DES302', 'IAG', 'Introduction to Animation and Graphics', [], [], [monsoon], [des]).
course('DES518', 'IMG', 'Introduction to Motion Graphics', [], [], [monsoon], [des]).
course('DES130', 'PIS', 'Prototyping Interactive Systems', [], [], [monsoon], [des]).
course('DES202', 'VDC', 'Visual Design & Communication', ['DES101'], [], [monsoon], [des]).
course('DES513', 'WARDI', 'Wearable Applications, Research, Devices, Interactions', [], [], [monsoon], [des]).
course('DES506', 'ATHCC', 'Advanced topics in Human Centered Computing (ATHCC)', ['DES204'], [], [winter], [des]).
course('DES101', 'DDV', 'Design Drawing & Visualization', [], [], [winter], [des]).
course('DES507', 'FVE', 'Fundamentals of Video for Engineers', [], [], [winter], [des]).
course('DES512', 'GDD', 'Game Design and Development', [], [], [winter], [des]).
course('DES204', 'HCI', 'Human Computer Interaction', [], ['CSE501'], [winter], [des]).
course('DES502', 'I2D', 'Introduction to 2D Animation', [], [], [winter], [des]).
course('DES516s', 'I3D', 'Introduction to 3D Animation', [], ['DES302'], [winter], [des]).
course('DES503', 'I3DA', 'Introduction to 3D Character Animation', ['DES302'], [], [winter], [des]).
course('DES102', 'IHCI', 'Introduction to HCI', [], [], [winter], [des]).
course('DES504', 'NVC', 'Narratives in Visual Communication', [], [], [winter], [des]).

% SSH Courses
course('ENT421/BIO571', 'HIEE', 'Healthcare Innovation and Entrepreneurship Essentials', [], [], [monsoon], [ssh]).
course('SOC311/SOC501', 'AERM', 'Advance Ethnographic Research Methods', ['SSH201'], [], [monsoon], [ssh]).
course('ECO314', 'BEco', 'Behavioural Economics', ['MTH201'], [], [monsoon], [ssh]).
course('SOC206', 'BA', 'Business Anthropology', [], [], [monsoon], [ssh]).
course('PSY308', 'CMM', 'Cognition of Motor Movement', ['CSE101', 'PSY201'], [], [monsoon], [ssh]).
course('SOC209', 'CCS', 'Consumer Culture and Society', [], [], [monsoon], [ssh]).
course('SOC202', 'CISP', 'Contemporary India: Sociological Perspective', [], [], [monsoon], [ssh]).
course('ECO502', 'CT', 'Contract Theory', [], [], [monsoon], [ssh]).
course('ECO503', 'DT', 'Decision Theory', [], [], [monsoon], [ssh]).
course('ECO333', 'DRM', 'Derivatives and Risk Management', ['ECO331', 'ECO332'], [], [monsoon], [ssh]).
course('SOC307', 'DSR', 'Digital Social Research', [], [], [monsoon], [ssh]).
course('ECO221', 'ECO', 'Econometrics I', ['MTH201'], ['ECO302'], [monsoon], [ssh]).
course('ECO322/ECO522', 'Econ-2', 'Econometrics II', ['ECO221'], [], [monsoon], [ssh]).
course('MGT310', 'ESCM', 'Effective Supply Chain for E Commerce', [], [], [monsoon], [ssh]).
course('SOC513', 'ETB', 'Enhancement Technologies and the Body: Beyond the Human Form', ['SOC101'], [], [monsoon], [ssh]).
course('ECO331', 'FF', 'Foundations of Finance', [], ['FIN401'], [monsoon], [ssh]).
course('ECO311/ECO511', 'GMT', 'Game Theory', [], ['ECO304'], [monsoon], [ssh]).
course('SOC308/SOC514', 'GM', 'Gender and Media', [], [], [monsoon], [ssh]).
course('SOC315/SOC511', 'INST', 'Intersectionality Studies', [], [], [monsoon], [ssh]).
course('SSH216', 'IIM', 'Introduction to Indian Mythology', [], ['SSH219'], [monsoon], [ssh]).
course('SSH121', 'IPHI', 'Introduction to Philosophy', [], [], [monsoon], [ssh]).
course('PSY201', 'Psy', 'Introduction to Psychology', ['SSH101'], [], [monsoon], [ssh]).
course('SOC212', 'KCES', 'Key Concepts in Economic Sociology', [], [], [monsoon], [ssh]).
course('SSH700', 'KPCC', 'Knowledge Production, Concepts and Context', [], [], [monsoon], [ssh]).
course('ECO201', 'MA', 'Macroeconomics', [], [], [monsoon], [ssh]).
course('ECO341/ECO541', 'MDP', 'Markov Decision Processes', ['MTH100', 'MTH201', 'MTH203'], [], [monsoon], [ssh]).
course('PSY307', 'NDM', 'Neuroscience of Decision Making', [], [], [monsoon], [ssh]).
course('SSH323', 'NMP', 'New Media and Politics', [], [], [monsoon], [ssh]).
course('SSH223', 'PoK', 'Perspectives of Knowledge', [], [], [monsoon], [ssh]).
course('SSH201', 'RMSSD', 'Research Methods in Social Science and Design', ['SSH101'], [], [monsoon], [ssh]).
course('SSH221', 'SPP', 'Social and Political Philosophy', [], [], [monsoon], [ssh]).
course('SOC204', 'SNM', 'Sociology of New Media', [], [], [monsoon], [ssh]).
course('SOC314/SOC512', 'TFM', 'Technology and the Future of Work', [], [], [monsoon], [ssh]).
course('SSH211', 'TA', 'Theatre Appreciation', [], ['SSH211'], [monsoon], [ssh]).
course('SSH300', 'TE', 'Theories of Ethics', [], [], [monsoon], [ssh]).
course('ECO301', 'IEA', 'Introduction to Economic Analysis', [], [], [monsoon, winter], [ssh]).
course('SSH362/SSH562', 'USPP', 'Urban Space and Political Power', [], [], [monsoon, winter], [ssh]).
course('SOC312/SOC502', 'AST', 'Advanced Sociological Theory', [], [], [winter], [ssh]).
course('SSH311', 'AW', 'Advanced Writing', [], [], [winter], [ssh]).
course('SOC205', 'ASM', 'Anthropology and Social Media', [], [], [winter], [ssh]).
course('SSH321', 'AE', 'Applied Ethics', [], [], [winter], [ssh]).
course('PSY305', 'ATP', 'Attention and Perception', [], [], [winter], [ssh]).
course('PSY301', 'CP', 'Cognitive Psychology', ['PSY201'], [], [winter], [ssh]).
course('SSH361', 'CPDE', 'Comparative Politics in Digital Era', [], [], [winter], [ssh]).
course('SSH224', 'CIES', 'Computer, Information Ethics and Society', [], [], [winter], [ssh]).
course('SSH124', 'CT', 'Critical Thinking', [], ['SSH101'], [winter], [ssh]).
course('SSH101', 'CTRSS', 'Critical Thinking and Readings in Social Science', [], [], [winter], [ssh]).
course('SSH261', 'DIPP', 'Democracy in India: Principles and Practices', [], [], [winter], [ssh]).
course('MGT311', 'EIITM', 'Emerging Issues in IT Management', [], [], [winter], [ssh]).
course('SSH325/SSH525', 'EI', 'Ethics in AI', [], [], [winter], [ssh]).
course('SSH217', 'IPA', 'Indian Poetry Through the Ages', [], [], [winter], [ssh]).
course('ECO312/ECO512', 'IO', 'Industrial Organization', ['ECO101', 'ECO311'], [], [winter], [ssh]).
course('SOC207', 'ITS', 'Information Technology And Society', [], ['SSH212', 'SSH101'], [winter], [ssh]).
course('SOC213', 'IDE', 'Introduction to Digital Ethnography', [], [], [winter], [ssh]).
course('SOC101', 'ISA', 'Introduction to Sociology and Anthropology', ['SSH101'], ['SSH205', 'SSH215'], [winter], [ssh]).
course('SSH214', 'Lit', 'Introduction To The Study Of Literature', [], [], [winter], [ssh]).
course('PSY306', 'LM', 'Learning and Memory', [], [], [winter], [ssh]).
course('ECO313', 'MD', 'Market Design', ['ECO311'], [], [winter], [ssh]).
course('ECO301', 'ME', 'Microeconomics', [], ['ECO101'], [winter], [ssh]).
course('ECO223', 'MB', 'Money and Banking', [], [], [winter], [ssh]).
course('SSH215', 'NN', 'Nation and her Narratives', [], [], [winter], [ssh]).
course('SOC208', 'OAP', 'Organizations An Anthropological Perspective', [], [], [winter], [ssh]).
course('SSH324', 'PoM', 'Philosophy of Mind', ['SSH121'], [], [winter], [ssh]).
course('SSH3PT', 'PT', 'Philosophy of Technology', [], [], [winter], [ssh]).
course('PSY202', 'Ppsy', 'Positive Psychology', [], [], [winter], [ssh]).
course('SOC211', 'STS', 'Science, Technology and Society', [], [], [winter], [ssh]).
course('PSY302', 'SP', 'Social Psychology', [], [], [winter], [ssh]).
course('SOC210', 'ST1', 'Sociological Theory', [], [], [winter], [ssh]).
course('SOC313/SOC503', 'SoI', 'Sociology of India: Themes and Perspectives', [], [], [winter], [ssh]).
course('ECO324/ECO524', 'SSSE', 'Spatial Statistics and Spatial Econometrics', ['ECO221'], [], [winter], [ssh]).
course('SSH222', 'TPEE', 'Theory and Practice of Engineering Ethics', [], [], [winter], [ssh]).
course('SOC302', 'US', 'Urban Sociology', [], [], [winter], [ssh]).
course('ECO332', 'VPM', 'Valuation and Portfolio Management', [], [], [winter], [ssh]).

% Other courses
course('COM101', 'COM', 'Communication Skills', [], [], [monsoon], [oth]).
course('ESC207A', 'EEE', 'Ecology, Evolution, and Environment', [], ['ESC205'], [monsoon], [oth]).
course('ENT411', 'EComm', 'Entrepreneurial Communication', [], [], [monsoon], [oth]).
course('ENT412', 'EK', 'Entrepreneurial Khichadi', [], [], [monsoon], [oth]).
course('ESC205', 'EVS', 'Environmental Sciences', [], ['ESC207A'], [monsoon], [oth]).
course('ENT421/BIO571', 'HIEE', 'Healthcare Innovation and Entrepreneurship Essentials', [], [], [monsoon], [oth]).
course('COM301A', 'TCOM', 'Technical Communication', [], [], [monsoon], [oth]).
course('ENT416/ENT516', 'CIIPS', 'Creativity, Innovation, and Inventive Problem Solving', [], [], [winter], [oth]).
course('ENT413', 'EF', 'Entrepreneurial Finance', [], [], [winter], [oth]).
course('ENT415', 'NVP', 'New Venture Planning', [], [], [winter], [oth]).
course('ENT414', 'RIPS', 'Relevance of Intellectual Property for Startups', [], [], [winter], [oth]).
course('ENG599s', 'RM', 'Research Methods', [], [], [winter], [oth]).

