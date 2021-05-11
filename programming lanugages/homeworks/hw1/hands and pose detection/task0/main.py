import json

import cv2
import mediapipe
from mediapipe.python.solutions.hands import HandLandmark

mp_drawing = mediapipe.solutions.drawing_utils
mp_hands = mediapipe.solutions.hands


def get_coordinates(landmark):
    coordinates = {}
    for landmark_id in HandLandmark:
        i = landmark_id.value
        x, y, z = landmark[i].x, landmark[i].x, landmark[i].y
        coordinates[landmark_id.name] = {'x': x, 'y': y, 'z': z}
    return json.dumps(coordinates)


def main():
    cap = cv2.VideoCapture(0)
    with mp_hands.Hands(
            max_num_hands=4,
            min_detection_confidence=0.5,
            min_tracking_confidence=0.5) as hands:
        while cap.isOpened():
            success, image = cap.read()
            if not success:
                continue

            image = cv2.cvtColor(cv2.flip(image, 1), cv2.COLOR_BGR2RGB)
            image.flags.writeable = False
            results = hands.process(image)

            image.flags.writeable = True
            image = cv2.cvtColor(image, cv2.COLOR_RGB2BGR)
            if results.multi_hand_landmarks:
                for hand_landmarks in results.multi_hand_landmarks:
                    print(get_coordinates(hand_landmarks.landmark))
                    mp_drawing.draw_landmarks(
                        image, hand_landmarks, mp_hands.HAND_CONNECTIONS)
            cv2.imshow('Hands', image)
            if cv2.waitKey(5) & 0xFF == 27:
                break
    cap.release()


if __name__ == '__main__':
    main()
