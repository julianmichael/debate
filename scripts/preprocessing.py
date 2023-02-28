import argparse
import json
import csv

parser = argparse.ArgumentParser(
    description='needed for selection/preprocessing of Q&As from QuALITY')
parser.add_argument("--quality_file",
                    help="where the QuALITY data is located "
                    "for example data/QuALITY.v1.0.1/QuALITY.v1.0.1",
                    required=True)
parser.add_argument("--debate_file",
                    help="where the single turn debate data is located "
                    "for example data/single_turn_debate_results.csv",
                    required=True)
parser.add_argument("--source",
                    help="source of the QuALITY stories, which can be chosen from: Gutenberg, Slate, "
                    "misc-freesouls, misc-longshort, misc-openaccess")
parser.add_argument("--min_untimed_accuracy",
                    help="the minimum required accuracy of untimed annotators against the writer's label.",
                    required=True, type=float)
parser.add_argument("--max_timed_accuracy",
                    help='the maximum allowed accuracy of the timed annotators against the gold label.',
                    required=True, type=float)

args = parser.parse_args()


# LOAD DATA
quality_story_data = {}
# for all QuALITY splits, uncomment and add for loop
quality_files = [args.quality_file +
                 ext for ext in ['.train', '.dev', '.test']]
# quality_file = args.quality_file + '.dev'
for quality_file in quality_files:
    with open(quality_file, encoding='utf-8',) as f:
        for line in f:
            story = json.loads(line)
            article_id = story['article_id']
            if article_id in quality_story_data:
                quality_story_data[article_id]['questions'].extend(
                    story['questions'])
            else:
                quality_story_data[article_id] = story
print("# of QuALITY stories in chosen QuALITY dataset:", len(quality_story_data))

if not quality_story_data:  # is this necessary?
    exit(1)

with open(args.debate_file, "r", encoding='utf-8', errors="surrogateescape") as debate_file:
    debate_story_data = list(csv.DictReader(debate_file, delimiter=","))
debate_file.close()
debate_story_ids = set([d['passage_id'] for d in debate_story_data])

print("# of QuALITY stories used in single turn debate:",
      len(debate_story_ids))

# PART 1: SUBSETTING STORIES
# Stories in QuALITY dataset AND Single Turn Debate (https://github.com/nyu-mll/single_turn_debate)
quality_debate_story_data = [quality_story_data[key]
                             for key in quality_story_data if key in debate_story_ids]

print("# of QuALITY stories in chosen dataset AND single turn debate:",
      len(quality_debate_story_data))

# Stories from overlap above AND from chosen source # We used Gutenberg only stories
if args.source:
    final_story_data = [
        story for story in quality_debate_story_data if story['source'] == args.source]
else:
    final_story_data = quality_debate_story_data

print("# of QuALITY stories from overlap above AND only from", args.source, ": ",
      len(final_story_data))

# PART 2: SUBSETTING QUESTIONS
# PART 2.1: Questions metadata - measures of agreement/correctness of untimed & timed annotators with labels
num_all_questions = sum([len(story['questions'])
                        for story in final_story_data])
print("\nThere are", num_all_questions, "questions from those stories")

untimed_accuracy = args.min_untimed_accuracy
timed_accuracy = args.max_timed_accuracy

for story in final_story_data:
    for question in story['questions']:
        validation = question['validation']
        num_correct = len(
            [v for v in validation if v['untimed_answer'] == question['writer_label']])
        question['untimed_accuracy'] = num_correct / len(validation)

num_golden_questions = len([question for story in final_story_data for question in story['questions']
                            if question['untimed_accuracy'] >= untimed_accuracy])
print("\nOut of those", num_all_questions, "questions,", num_golden_questions,
      "questions have a untimed accuracy equal to or greater than \nthe untimed accuracy of", untimed_accuracy,
      "that you wanted")

for story in final_story_data:
    for question in story['questions']:
        validation = question['speed_validation']
        num_correct = len(
            [v for v in validation if v['speed_answer'] == question['gold_label']])
        question['timed_accuracy'] = num_correct / len(validation)

num_difficult_questions = len([question for story in final_story_data for question in story['questions']
                               if question['timed_accuracy'] <= timed_accuracy])
print("\nOut of those", num_all_questions, "questions,", num_difficult_questions,
      "questions have a timed accuracy equal to or LESS than \nthe timed accuracy of", timed_accuracy,
      "that you wanted")

num_golden_difficult_questions = len([question for story in final_story_data for question in story['questions']
                                      if question['untimed_accuracy'] >= untimed_accuracy and question['timed_accuracy'] <= timed_accuracy])
print("\nFinally, out of those", num_all_questions, "questions,", num_golden_difficult_questions,
      "questions fufill both requirements of untimed accuracy and timed accuracy")


def char_replacement_distance(x, y):
    length_diff = abs(len(x) - len(y))
    edit_diff = 0
    for i in range(0, min(len(x), len(y))):
        if x[i] != y[i]:
            edit_diff += 1
    return edit_diff + length_diff


def print_questions_with_no_quality_match_for_char_distance(dist: int):
    num_questions_no_quality_match = 0
    print("\nThe following questions in the debate data have no match in QuALITY:")
    for story in final_story_data:
        # print(story['article_id'])
        debate_questions = list(
            set([debate_story['question_text'] for debate_story in debate_story_data if debate_story['passage_id'] == story['article_id']]))
        for dquestion in debate_questions:
            quality_official_questions = []
            for q in story['questions']:
                q['question'] = q['question'].replace("’", "'")
                # q['question'] = q['question'].strip()
                quality_official_questions = quality_official_questions + \
                    [q['question']]
            any_q_missing = False

            if min(char_replacement_distance(dquestion, q) for q in quality_official_questions) == dist:
                if dquestion not in quality_official_questions:
                    any_q_missing = True
                    num_questions_no_quality_match += 1
            if any_q_missing:
                print(story['article_id'])
                print(f"Debate: {dquestion}")
                print(*quality_official_questions, sep='\n')

    print(
        f"Number of questions with no match in QuALITY for dist {dist}: {num_questions_no_quality_match}")


for i in range(0, 20, 1):
    print_questions_with_no_quality_match_for_char_distance(i)
