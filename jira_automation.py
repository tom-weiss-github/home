import argparse
from atlassian import Jira
from datetime import datetime, timedelta

# --- Configuration ---
JIRA_URL = "https://tradingtech.atlassian.net"
USERNAME = "tom.weiss@tradingtechnologies.com"
with open("/home/tweiss/jira_token") as tfile:
    API_TOKEN = tfile.readline().strip()

BOARD_ID = 217

# Initialize Jira connection
jira = Jira(
    url=JIRA_URL,
    username=USERNAME,
    password=API_TOKEN,
    cloud=True
)


def get_current_sprints(board_id):
    """
    board_id: The Jira board id (use the Jira website to find the id).

    Returns a dictionary whose keys are sprint names (prefixes) and value is a list of sprints.
    """
    result = dict()
    sprints = jira.get_all_sprint(board_id, state='active,future')
    for sprint in sprints.get('values', []):
        # print("name='{}' state='{}'".format(sprint.get('name'),
        #                                     sprint.get('state')))
        title, enddate = sprint.get('name').rsplit(' ', 1)
        if title not in result:
            result[title] = list()
        result[title].append(sprint.get('name'))
    return result


def create_next_sprint_name(name):
    """
    Create a sprint name based on the current name.  Given a date like 'Deployment 01Apr2026' create
    a python function that will create a similar date two weeks in the future (i.e., 'Deployment
    15Apr2026').
    """
    title, enddate = name.rsplit(' ', 1)
    current_date = datetime.strptime(enddate, "%d%b%Y")
    next_name = current_date + timedelta(weeks=2)
    return "{} {}".format(title, next_name.strftime("%d%b%Y"))


def create_new_sprint(board_id, sprint_name, days_duration=14):
    title, enddate = sprint_name.rsplit(' ', 1)
    end_date = datetime.strptime(enddate, "%d%b%Y")
    end_date = end_date + timedelta(hours=13)
    start_date = end_date - timedelta(weeks=2)

    # Format dates for Jira API (ISO 8601)
    start_str = start_date.strftime('%Y-%m-%dT%H:%M:%S.%f')[:-3] + 'Z'
    end_str = end_date.strftime('%Y-%m-%dT%H:%M:%S.%f')[:-3] + 'Z'

    try:
        response = jira.create_sprint(
            board_id=board_id,
            name=sprint_name,
            start_date=start_str,
            end_date=end_str
        )
        print(f"Successfully created sprint: {sprint_name} (ID: {response['id']})")
        return response
    except Exception as e:
        print(f"Failed to create sprint: {e}")


if __name__ == "__main__":
    descr = ("Tool for managing Jira sprints.")
    parser = argparse.ArgumentParser(description=descr)
    parser.add_argument('-b', '--boards', action='store', nargs="+",
                        default=["217", "545"],
                        help=("A space separated list of board ids, if not provided the default is"
                              "'217 545'."))
    parser.add_argument('-a', '--action', action='store', choices=['list', 'create'],
                        help=("The action to take."))
    args = parser.parse_args()

    if args.action == 'list':
        for board in args.boards:
            sprints = get_current_sprints(board)
            print("\nBoard {}".format(board))
            for sprint,sprint_names in sprints.items():
                print("\n".join(sprint_names))

    elif args.action == 'create':
        for board in args.boards:
            sprints = get_current_sprints(board)
            for sprint,sprint_names in sprints.items():
                last_sprint_name = sprint_names[-1]
                next_sprint_name = create_next_sprint_name(last_sprint_name)

                print("Board {}'s latest is '{}', next is '{}'.".format(
                    board, last_sprint_name, next_sprint_name))
                answer = input("Continue (y/n): ")
                if answer in ['y', 'yes']:
                    print("Creating sprint '{}'".format(next_sprint_name))
                    create_new_sprint(board, next_sprint_name)
                else:
                    print("(no action taken)")
