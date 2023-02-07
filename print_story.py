import sys
import json
import re

file = sys.argv[1]
title = sys.argv[2]

story_data = {}
i = 0
with open(file) as f:
    for line in f:
        story = json.loads(line)
        article_id = story['article_id']
        if article_id in story_data:
            story_data[article_id]['questions'].extend(story['questions'])
        else:
            story_data[article_id] = story

        print(type(story['questions']))
        print(len(story_data))
        i = i + 1
        if i >= 5:
            exit(0)

story_data = []
with open(file) as f:
    for line in f:
        tmp_story_data = json.loads(line)
        if title.lower() == tmp_story_data['title'].lower():
            story_data = story_data + [tmp_story_data]

if not story_data:
    exit(1)


spacing_tags = [
    '<h1>', '<h2>',
    '</h1>', '</h2>', '</p>', '<p>',
    '<i>', '</i>', '<html>', '</html>',
    '<p class="ph3">'
]

# print(story)
story = story_data[0]['article']
prev_story_iter = None
story = re.sub(r'[\n ]+<i>[\n ]+(.*?)[\n ]+</i>[\n ]+', ' *\\1* ', story)
story = re.sub(r'[\n ]+<br/>[\n ]+', r'\n', story)
for tag in spacing_tags:
    story = story.replace(tag, '\n')
story = story.replace('<hr class="chap"/>', '===============')
story = story.replace('<hr class="tb"/>', '---------------')
while prev_story_iter != story:
    prev_story_iter = story
    story = story.replace('\n ', '\n')
    story = story.replace(' \n', '\n')
    story = story.replace('\n\n\n', '\n\n')
    # comment the next 3 lines if you want to preserve line breaks
    story = story.replace('\n\n', '<keepbr\>')
    story = story.replace('\n', ' ')
    story = story.replace('<keepbr\>', '\n\n')
for set in story_data:
    for q_json in set['questions']:
        if q_json['difficult'] > 0:
            print('(Difficult:) ', end='')
        print(q_json['question'])
        for option in q_json['options']:
            print('\t' + option)
