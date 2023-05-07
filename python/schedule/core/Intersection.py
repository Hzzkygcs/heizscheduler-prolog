from enum import Enum


class Intersection(Enum):
    INTERSECT = 0
    CONTAINS = 1
    INSIDE = 2

    BEFORE = 10
    AFTER = 11

    def intersects_or_contains(self):
        return self in (
            Intersection.INTERSECT,
            Intersection.CONTAINS,
            Intersection.INSIDE,
        )
