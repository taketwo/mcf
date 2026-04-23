import pytest

from synctank.schema import Kind, Status


class TestKind:
    def test_all_values_are_strings(self) -> None:
        for kind in Kind:
            assert isinstance(kind.value, str)

    @pytest.mark.parametrize("kind", [k for k in Kind if k != Kind.OTHER])
    def test_non_other_has_filename_suffix(self, kind: Kind) -> None:
        assert kind.has_filename_suffix is True

    def test_other_has_no_filename_suffix(self) -> None:
        assert Kind.OTHER.has_filename_suffix is False

    @pytest.mark.parametrize("kind", list(Kind))
    def test_all_kinds_have_description(self, kind: Kind) -> None:
        assert isinstance(kind.description, str)
        assert len(kind.description) > 0

    def test_lessons_learned_value(self) -> None:
        assert Kind.LESSONS_LEARNED.value == "lessons-learned"

    @pytest.mark.parametrize(
        ("value", "expected"),
        [
            ("brief", Kind.BRIEF),
            ("design", Kind.DESIGN),
            ("requirements", Kind.REQUIREMENTS),
            ("spec", Kind.SPEC),
            ("plan", Kind.PLAN),
            ("report", Kind.REPORT),
            ("brainstorm", Kind.BRAINSTORM),
            ("lessons-learned", Kind.LESSONS_LEARNED),
            ("guide", Kind.GUIDE),
            ("reference", Kind.REFERENCE),
            ("other", Kind.OTHER),
        ],
    )
    def test_construct_from_value(self, value: str, expected: Kind) -> None:
        assert Kind(value) == expected


class TestStatus:
    def test_all_values_are_strings(self) -> None:
        for status in Status:
            assert isinstance(status.value, str)

    @pytest.mark.parametrize("status", list(Status))
    def test_all_statuses_have_description(self, status: Status) -> None:
        assert isinstance(status.description, str)
        assert len(status.description) > 0

    @pytest.mark.parametrize(
        ("value", "expected"),
        [
            ("draft", Status.DRAFT),
            ("living", Status.LIVING),
            ("complete", Status.COMPLETE),
            ("superseded", Status.SUPERSEDED),
        ],
    )
    def test_construct_from_value(self, value: str, expected: Status) -> None:
        assert Status(value) == expected
