def new_remote_control_car:
  {
    battery_percentage: 100,
    distance_driven_in_meters: 0,
    nickname: null
  }
;

def new_remote_control_car(nickname):
  new_remote_control_car
  | .nickname = nickname
;

def display_distance:
  "\(.distance_driven_in_meters) meters"
;

def display_battery:
  if .battery_percentage == 0 then "empty"
  else "at \(.battery_percentage)%"
  end
  | "Battery \(.)"
;

def drive:
  (
    if .battery_percentage == 0 then
      {}
    else
      {
        battery_percentage: (.battery_percentage - 1),
        distance_driven_in_meters: (.distance_driven_in_meters + 20)
      }
    end
  ) as $mod
  | . + $mod
;
