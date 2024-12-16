chmod -R 777 /opt

# Check if the initialization has been done
if [ ! -f /opt/initialized ]; then
  echo "Initializing ejabberd..."

  # Check if ejabberd is already running (if not, we start it)
  if ! ejabberdctl status > /dev/null 2>&1; then
    echo "ejabberd is not running, starting ejabberd..."
    ejabberdctl start
  fi

  # Wait for ejabberd to be fully started
  echo "Waiting for ejabberd to start..."
  until ejabberdctl status > /dev/null 2>&1; do
    sleep 1
  done


  if ! ejabberdctl registered_users | grep -q "admin@localhost"; then
    ejabberdctl register admin localhost adminpassword
  fi

  # Add users (if they don't already exist)
  if ! ejabberdctl registered_users | grep -q "user1@localhost"; then
    ejabberdctl register user1 localhost user1password
  fi

  if ! ejabberdctl registered_users | grep -q "user2@localhost"; then
    ejabberdctl register user2 localhost user2password
  fi

  if ! ejabberdctl registered_users | grep -q "user3@localhost"; then
    ejabberdctl register user3 localhost user3password
  fi

  if ! ejabberdctl registered_users | grep -q "user4@localhost"; then
    ejabberdctl register user4 localhost user4password
  fi

  if ! ejabberdctl registered_users | grep -q "user5@localhost"; then
    ejabberdctl register user5 localhost user5password

  if ! ejabberdctl registered_users | grep -q "user6@localhost"; then
    ejabberdctl register user6 localhost user6password
  fi

  if ! ejabberdctl registered_users | grep -q "user7@localhost"; then
    ejabberdctl register user7 localhost user7password
  fi

  if ! ejabberdctl registered_users | grep -q "user8@localhost"; then
    ejabberdctl register user8 localhost user8password
  fi

  if ! ejabberdctl registered_users | grep -q "user9@localhost"; then
    ejabberdctl register user9 localhost user9password
  fi

  if ! ejabberdctl registered_users | grep -q "user10@localhost"; then
    ejabberdctl register user10 localhost user10password
  fi

  users="user1 user2 user3 user4 user5 user6 user7"

  # Convert the string into an array-like list
  set -- $users

  # Outer loop: Iterate over each user
  for user1; do
    shift # Remove the current user1 from the list
    # Inner loop: Iterate over remaining users
    for user2; do
      # Avoid adding the user to their own roster
      if [ "$user1" != "$user2" ]; then
        echo "Add $user1 and $user2 to each other roaster"
        # Add user2 to user1's roster
        ejabberdctl add_rosteritem "$user1" localhost "$user2" localhost "$user2" friends,colleagues both
      fi
    done
  done

  echo "All users have been added to each other's rosters."

  # Create a chat room (if it doesn't already exist)
  if ! ejabberdctl get_room_occupants_number chatroom1 conference.localhost > /dev/null 2>&1; then
    echo "Create 'chatroom1'"
    ejabberdctl create_room_with_opts chatroom1 conference.localhost localhost persistent:true
    
    # Add user1 to chatroom1 as member
    echo "Add user1 to chatroom1 as member (affiliation)"
    ejabberdctl set_room_affiliation chatroom1 conference.localhost user1@localhost member
    
    # Add user2 to chatroom1 as member
    echo "Add user2 to chatroom1 as member (affiliation)"
    ejabberdctl set_room_affiliation chatroom1 conference.localhost user2@localhost member
    
    # Add user3 to chatroom1 as admin
    echo "Add user3 to chatroom1 as admin (affiliation)"
    ejabberdctl set_room_affiliation chatroom1 conference.localhost user3@localhost admin
    
    # Add user4 to chatroom1 as owner
    echo "Add user4 to chatroom1 as owner (affiliation)"
    ejabberdctl set_room_affiliation chatroom1 conference.localhost user4@localhost owner
    
    # Add user5 to chatroom1 as outcast
    echo "Add user5 to chatroom1 as outcast (affiliation)"
    ejabberdctl set_room_affiliation chatroom1 conference.localhost user5@localhost outcast

    echo "Add user6 to chatroom1 as member (affiliation)"
    ejabberdctl set_room_affiliation chatroom1 conference.localhost user6@localhost member

    echo "Add user7 to chatroom1 as member (affiliation)"
    ejabberdctl set_room_affiliation chatroom1 conference.localhost user7@localhost member

    echo "Add user8 to chatroom1 as member (affiliation)"
    ejabberdctl set_room_affiliation chatroom1 conference.localhost user8@localhost member

    echo "Add user9 to chatroom1 as member (affiliation)"
    ejabberdctl set_room_affiliation chatroom1 conference.localhost user9@localhost member

    echo "Add user10 to chatroom1 as member (affiliation)"
    ejabberdctl set_room_affiliation chatroom1 conference.localhost user10@localhost member
  fi

  # Mark initialization as done
  touch /opt/initialized
  echo "Initialization complete."
  ejabberdctl stop
  echo "Waiting for ejabberd to stop...(will be started as docker main process soon)"
  until ! ejabberdctl status > /dev/null 2>&1; do
    sleep 1
  done
  echo "ejabberd has stopped"
else
  echo "ejabberd already initialized."
fi
