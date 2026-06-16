{ pkgs, ... }:

let
  python = pkgs.python3.withPackages (ps: [
    ps.numpy
    ps.pillow
    ps.pydicom
  ]);

  inventoryPy = pkgs.writeText "dicom-inventory.py" ''
    #!/usr/bin/env python3
    import argparse
    import json
    import re
    import signal
    import sys
    import warnings
    from collections import Counter
    from pathlib import Path
    from zipfile import ZipFile, BadZipFile

    signal.signal(signal.SIGPIPE, signal.SIG_DFL)
    warnings.filterwarnings(
        "ignore",
        message="Incorrect value for Specific Character Set.*",
        category=UserWarning,
        module="pydicom.charset",
    )

    import pydicom
    from pydicom.multival import MultiValue

    SKIP_EXTENSIONS = {
        ".bmp", ".dll", ".exe", ".gif", ".ico", ".ini", ".jpg", ".jpeg",
        ".lic", ".local", ".log", ".manifest", ".ocx", ".png", ".profiles",
        ".txt", ".xml",
    }

    SPECIFIC_TAGS = [
        "StudyInstanceUID", "SeriesInstanceUID", "SOPInstanceUID",
        "Modality", "BodyPartExamined", "StudyDescription", "SeriesDescription",
        "ProtocolName", "SeriesNumber", "InstanceNumber", "ImageType",
        "SliceThickness", "PixelSpacing", "SpacingBetweenSlices",
        "Rows", "Columns", "Manufacturer", "ManufacturerModelName",
        "ContrastBolusAgent", "ContrastBolusRoute", "BurnedInAnnotation",
    ]

    CONTRAST_RE = re.compile(r"(?<!\w)(w\+|c\+|post|contrast|ce|arterial|venous|portal|delayed)(?!\w)", re.I)

    def clean(value):
        if value is None:
            return None
        if isinstance(value, bytes):
            return "<bytes>"
        if isinstance(value, (list, tuple, MultiValue)):
            return "\\".join(str(v) for v in value)
        text = str(value).strip()
        return text if text else None

    def add_value(target, value):
        value = clean(value)
        if value:
            target.add(value)

    def dicom_candidates(path):
        path = Path(path).expanduser()
        if path.is_file() and path.suffix.lower() == ".zip":
            with ZipFile(path) as zf:
                for info in zf.infolist():
                    if info.is_dir():
                        continue
                    name = info.filename
                    suffix = Path(name).suffix.lower()
                    basename = Path(name).name.upper()
                    if suffix in SKIP_EXTENSIONS and basename != "DICOMDIR":
                        continue
                    yield "zip", zf, name
        elif path.is_dir():
            for child in path.rglob("*"):
                if not child.is_file():
                    continue
                suffix = child.suffix.lower()
                basename = child.name.upper()
                if suffix in SKIP_EXTENSIONS and basename != "DICOMDIR":
                    continue
                yield "file", None, child
        else:
            yield "file", None, path

    def read_dataset(kind, container, item):
        try:
            if kind == "zip":
                with container.open(item) as fp:
                    return pydicom.dcmread(
                        fp,
                        stop_before_pixels=True,
                        force=False,
                        specific_tags=SPECIFIC_TAGS,
                    )
            return pydicom.dcmread(
                str(item),
                stop_before_pixels=True,
                force=False,
                specific_tags=SPECIFIC_TAGS,
            )
        except Exception:
            return None

    def path_text(item):
        return str(item)

    def build_inventory(input_path, show_paths, show_values):
        series = {}
        stats = Counter()
        errors = 0

        try:
            candidates = dicom_candidates(input_path)
            for kind, container, item in candidates:
                stats["candidate_files"] += 1
                ds = read_dataset(kind, container, item)
                if ds is None:
                    errors += 1
                    continue

                if path_text(item).upper().endswith("DICOMDIR"):
                    stats["dicomdir_files"] += 1
                    continue

                stats["dicom_files"] += 1
                raw_series_uid = clean(getattr(ds, "SeriesInstanceUID", None))
                path_group_key = f"path:{Path(path_text(item)).parent}"
                series_key = raw_series_uid or path_group_key
                series_uid = raw_series_uid or (path_group_key if show_paths else "<missing; path-derived grouping hidden>")
                study_uid = clean(getattr(ds, "StudyInstanceUID", None)) or "<missing>"
                if not show_values:
                    series_uid = "<hidden; rerun with --show-values>" if raw_series_uid else series_uid
                    study_uid = "<hidden; rerun with --show-values>" if study_uid != "<missing>" else study_uid

                if series_key not in series:
                    series[series_key] = {
                        "study_uid": study_uid,
                        "series_uid": series_uid,
                        "count": 0,
                        "first_path": path_text(item) if show_paths else "<hidden; rerun with --show-paths>",
                        "modalities": set(),
                        "body_parts": set(),
                        "study_descriptions": set(),
                        "series_descriptions": set(),
                        "protocol_names": set(),
                        "series_numbers": set(),
                        "image_types": set(),
                        "slice_thicknesses": set(),
                        "pixel_spacings": set(),
                        "spacing_between_slices": set(),
                        "matrix_sizes": set(),
                        "manufacturers": set(),
                        "models": set(),
                        "contrast_agents": set(),
                        "contrast_routes": set(),
                        "burned_in_annotations": set(),
                        "instance_numbers": [],
                    }

                row = series[series_key]
                row["count"] += 1
                add_value(row["modalities"], getattr(ds, "Modality", None))
                add_value(row["body_parts"], getattr(ds, "BodyPartExamined", None))
                add_value(row["study_descriptions"], getattr(ds, "StudyDescription", None))
                add_value(row["series_descriptions"], getattr(ds, "SeriesDescription", None))
                add_value(row["protocol_names"], getattr(ds, "ProtocolName", None))
                add_value(row["series_numbers"], getattr(ds, "SeriesNumber", None))
                add_value(row["image_types"], getattr(ds, "ImageType", None))
                add_value(row["slice_thicknesses"], getattr(ds, "SliceThickness", None))
                add_value(row["pixel_spacings"], getattr(ds, "PixelSpacing", None))
                add_value(row["spacing_between_slices"], getattr(ds, "SpacingBetweenSlices", None))
                add_value(row["manufacturers"], getattr(ds, "Manufacturer", None))
                add_value(row["models"], getattr(ds, "ManufacturerModelName", None))
                add_value(row["contrast_agents"], getattr(ds, "ContrastBolusAgent", None))
                add_value(row["contrast_routes"], getattr(ds, "ContrastBolusRoute", None))
                add_value(row["burned_in_annotations"], getattr(ds, "BurnedInAnnotation", None))

                rows = clean(getattr(ds, "Rows", None))
                cols = clean(getattr(ds, "Columns", None))
                if rows and cols:
                    row["matrix_sizes"].add(f"{rows}x{cols}")

                instance = clean(getattr(ds, "InstanceNumber", None))
                if instance:
                    try:
                        row["instance_numbers"].append(int(float(instance)))
                    except ValueError:
                        pass
        except BadZipFile as exc:
            raise SystemExit(f"Not a valid zip file: {exc}")

        output_series = []
        for row in series.values():
            text_for_hint = " ".join(
                sorted(row["series_descriptions"] | row["protocol_names"] | row["image_types"])
            )
            contrast_hint = bool(row["contrast_agents"] or row["contrast_routes"] or CONTRAST_RE.search(text_for_hint))
            instances = row.pop("instance_numbers")
            instance_span = None
            if instances:
                instance_span = [min(instances), max(instances)]

            converted = {
                key: sorted(value) if isinstance(value, set) else value
                for key, value in row.items()
            }
            if not show_values:
                hidden = ["<hidden; rerun with --show-values>"]
                converted["study_descriptions"] = hidden if converted["study_descriptions"] else []
                converted["series_descriptions"] = hidden if converted["series_descriptions"] else []
                converted["protocol_names"] = hidden if converted["protocol_names"] else []
            converted["contrast_hint"] = contrast_hint
            converted["instance_number_span"] = instance_span
            output_series.append(converted)

        output_series.sort(key=lambda r: (
            next(iter(r["series_numbers"]), "999999"),
            next(iter(r["series_descriptions"]), ""),
            r["series_uid"],
        ))

        return {
            "input": str(Path(input_path).expanduser()) if show_paths else "<hidden; rerun with --show-paths>",
            "stats": dict(stats),
            "parse_errors_or_non_dicom_candidates": errors,
            "series_count": len(output_series),
            "series": output_series,
        }

    def format_values(values):
        if not values:
            return "-"
        return "; ".join(values[:4]) + ("; ..." if len(values) > 4 else "")

    def print_markdown(inv):
        print("# DICOM Inventory")
        print()
        print(f"Input: `{inv['input']}`")
        print()
        print("## Summary")
        print()
        print(f"- DICOM image files parsed: {inv['stats'].get('dicom_files', 0)}")
        print(f"- DICOMDIR files: {inv['stats'].get('dicomdir_files', 0)}")
        print(f"- Series count: {inv['series_count']}")
        print(f"- Parse errors / skipped non-DICOM candidates: {inv['parse_errors_or_non_dicom_candidates']}")
        print()
        print("## Series")
        print()
        for idx, row in enumerate(inv["series"], 1):
            print(f"### {idx}. Series {format_values(row['series_numbers'])}")
            print()
            print(f"- Images: {row['count']}")
            print(f"- Modality: {format_values(row['modalities'])}")
            print(f"- Body part: {format_values(row['body_parts'])}")
            print(f"- Study description: {format_values(row['study_descriptions'])}")
            print(f"- Series description: {format_values(row['series_descriptions'])}")
            print(f"- Protocol: {format_values(row['protocol_names'])}")
            print(f"- Image type: {format_values(row['image_types'])}")
            print(f"- Matrix: {format_values(row['matrix_sizes'])}")
            print(f"- Pixel spacing: {format_values(row['pixel_spacings'])}")
            print(f"- Slice thickness: {format_values(row['slice_thicknesses'])}")
            print(f"- Spacing between slices: {format_values(row['spacing_between_slices'])}")
            print(f"- Instance span: {row['instance_number_span'] or '-'}")
            print(f"- Contrast hint: {'yes' if row['contrast_hint'] else 'no'}")
            print(f"- Burned-in annotation metadata: {format_values(row['burned_in_annotations'])}")
            print(f"- First file: `{row['first_path']}`")
            print()

    def main():
        parser = argparse.ArgumentParser(description="Inventory DICOM studies without reading pixel data.")
        parser.add_argument("input", help="DICOM zip, DICOMDIR export folder, or single DICOM file")
        parser.add_argument("--json", action="store_true", help="Emit JSON instead of Markdown")
        parser.add_argument("--show-paths", action="store_true", help="Print input/member paths; may expose identifiers")
        parser.add_argument("--show-values", action="store_true", help="Print study/series identifiers and descriptor values; may expose identifiers")
        args = parser.parse_args()

        inv = build_inventory(args.input, args.show_paths, args.show_values)
        if args.json:
            print(json.dumps(inv, indent=2, sort_keys=True))
        else:
            print_markdown(inv)

    if __name__ == "__main__":
        main()
  '';

  privacyPy = pkgs.writeText "dicom-privacy-check.py" ''
    #!/usr/bin/env python3
    import argparse
    import json
    import signal
    import sys
    import warnings
    from collections import Counter, defaultdict
    from pathlib import Path
    from zipfile import ZipFile, BadZipFile

    signal.signal(signal.SIGPIPE, signal.SIG_DFL)
    warnings.filterwarnings(
        "ignore",
        message="Incorrect value for Specific Character Set.*",
        category=UserWarning,
        module="pydicom.charset",
    )

    import pydicom

    SKIP_EXTENSIONS = {
        ".bmp", ".dll", ".exe", ".gif", ".ico", ".ini", ".jpg", ".jpeg",
        ".lic", ".local", ".log", ".manifest", ".ocx", ".png", ".profiles",
        ".txt", ".xml",
    }

    PHI_TAGS = [
        "PatientName", "PatientID", "PatientBirthDate", "PatientSex", "PatientAge",
        "PatientAddress", "PatientTelephoneNumbers", "OtherPatientIDs",
        "AccessionNumber", "StudyID", "StudyDate", "StudyTime", "SeriesDate",
        "SeriesTime", "AcquisitionDate", "AcquisitionTime", "ContentDate", "ContentTime",
        "InstitutionName", "InstitutionAddress", "InstitutionalDepartmentName",
        "ReferringPhysicianName", "PerformingPhysicianName", "OperatorsName",
        "StationName", "DeviceSerialNumber", "ProtocolName", "StudyDescription",
        "SeriesDescription", "BurnedInAnnotation",
    ]

    def clean(value):
        if value is None:
            return None
        if isinstance(value, bytes):
            return "<bytes>"
        text = str(value).strip()
        return text if text else None

    def dicom_candidates(path):
        path = Path(path).expanduser()
        if path.is_file() and path.suffix.lower() == ".zip":
            with ZipFile(path) as zf:
                for info in zf.infolist():
                    if info.is_dir():
                        continue
                    name = info.filename
                    suffix = Path(name).suffix.lower()
                    basename = Path(name).name.upper()
                    if suffix in SKIP_EXTENSIONS and basename != "DICOMDIR":
                        continue
                    yield "zip", zf, name
        elif path.is_dir():
            for child in path.rglob("*"):
                if not child.is_file():
                    continue
                suffix = child.suffix.lower()
                basename = child.name.upper()
                if suffix in SKIP_EXTENSIONS and basename != "DICOMDIR":
                    continue
                yield "file", None, child
        else:
            yield "file", None, path

    def read_dataset(kind, container, item):
        try:
            if kind == "zip":
                with container.open(item) as fp:
                    return pydicom.dcmread(
                        fp,
                        stop_before_pixels=True,
                        force=False,
                        specific_tags=PHI_TAGS,
                    )
            return pydicom.dcmread(
                str(item),
                stop_before_pixels=True,
                force=False,
                specific_tags=PHI_TAGS,
            )
        except Exception:
            return None

    def check(input_path, show_values, show_paths):
        counts = Counter()
        examples = defaultdict(list)
        values = defaultdict(list)
        total = 0
        parsed = 0
        errors = 0
        dicomdir_files = 0
        jpeg_previews = 0

        path = Path(input_path).expanduser()
        if path.is_file() and path.suffix.lower() == ".zip":
            with ZipFile(path) as zf:
                jpeg_previews = sum(
                    1 for name in zf.namelist()
                    if Path(name).suffix.lower() in {".jpg", ".jpeg", ".png"}
                )
        elif path.is_dir():
            jpeg_previews = sum(
                1 for child in path.rglob("*")
                if child.is_file() and child.suffix.lower() in {".jpg", ".jpeg", ".png"}
            )

        try:
            for kind, container, item in dicom_candidates(input_path):
                total += 1
                ds = read_dataset(kind, container, item)
                if ds is None:
                    errors += 1
                    continue
                if str(item).upper().endswith("DICOMDIR"):
                    dicomdir_files += 1
                    continue
                parsed += 1
                for tag in PHI_TAGS:
                    value = clean(getattr(ds, tag, None))
                    if value:
                        counts[tag] += 1
                        if show_paths and len(examples[tag]) < 3:
                            examples[tag].append(str(item))
                        if show_values and len(values[tag]) < 3:
                            values[tag].append(value[:120])
        except BadZipFile as exc:
            raise SystemExit(f"Not a valid zip file: {exc}")

        return {
            "input": str(path) if show_paths else "<hidden; rerun with --show-paths>",
            "candidate_files": total,
            "dicom_files_parsed": parsed,
            "dicomdir_files_present": dicomdir_files,
            "parse_errors_or_non_dicom_candidates": errors,
            "jpeg_or_png_preview_files": jpeg_previews,
            "metadata_fields_present": {
                tag: {
                    "count": counts[tag],
                    **({"example_paths": examples[tag]} if show_paths else {}),
                    **({"example_values": values[tag]} if show_values else {}),
                }
                for tag in sorted(counts)
            },
        }

    def print_markdown(result, show_values, show_paths):
        print("# DICOM Privacy Check")
        print()
        print(f"Input: `{result['input']}`")
        print()
        print("## Summary")
        print()
        print(f"- DICOM files parsed: {result['dicom_files_parsed']}")
        print(f"- DICOMDIR files present: {result['dicomdir_files_present']}")
        print(f"- Parse errors / skipped non-DICOM candidates: {result['parse_errors_or_non_dicom_candidates']}")
        print(f"- JPEG/PNG preview files present: {result['jpeg_or_png_preview_files']}")
        print()
        if result["dicomdir_files_present"]:
            print("DICOMDIR is present and may contain patient/study directory records. Treat it as sensitive unless separately anonymized.")
            print()
        if result["jpeg_or_png_preview_files"]:
            print("Preview images may contain burned-in labels or exported viewer annotations. Inspect before sharing.")
            print()
        print("## Metadata fields present")
        print()
        if not result["metadata_fields_present"]:
            print("No checked PHI/sensitive metadata fields were populated in parsed DICOM image files.")
            return
        for tag, info in result["metadata_fields_present"].items():
            print(f"### {tag}")
            print(f"- Files with value: {info['count']}")
            if show_paths:
                print("- Example paths:")
                for path in info["example_paths"]:
                    print(f"  - `{path}`")
            else:
                print("- Example paths hidden; rerun with `--show-paths` if local path disclosure is acceptable.")
            if show_values:
                print("- Example values:")
                for value in info.get("example_values", []):
                    print(f"  - `{value}`")
            print()

    def main():
        parser = argparse.ArgumentParser(description="Check selected DICOM metadata fields for possible identifiers.")
        parser.add_argument("input", help="DICOM zip, DICOMDIR export folder, or single DICOM file")
        parser.add_argument("--json", action="store_true", help="Emit JSON instead of Markdown")
        parser.add_argument("--show-values", action="store_true", help="Print example metadata values; may expose identifiers")
        parser.add_argument("--show-paths", action="store_true", help="Print input/member paths; may expose identifiers")
        args = parser.parse_args()

        result = check(args.input, args.show_values, args.show_paths)
        if args.json:
            print(json.dumps(result, indent=2, sort_keys=True))
        else:
            print_markdown(result, args.show_values, args.show_paths)

    if __name__ == "__main__":
        main()
  '';

  contactSheetsPy = pkgs.writeText "dicom-contact-sheets.py" ''
    #!/usr/bin/env python3
    import argparse
    import math
    import signal
    import tempfile
    import warnings
    from pathlib import Path
    from zipfile import ZipFile, BadZipFile

    import numpy as np
    from PIL import Image, ImageDraw

    signal.signal(signal.SIGPIPE, signal.SIG_DFL)
    warnings.filterwarnings(
        "ignore",
        message="Incorrect value for Specific Character Set.*",
        category=UserWarning,
        module="pydicom.charset",
    )

    import pydicom
    from pydicom.multival import MultiValue

    SKIP_EXTENSIONS = {
        ".bmp", ".dll", ".exe", ".gif", ".ico", ".ini", ".jpg", ".jpeg",
        ".lic", ".local", ".log", ".manifest", ".ocx", ".png", ".profiles",
        ".txt", ".xml",
    }

    WINDOWS = {
        "soft": (40.0, 400.0),
        "lung": (-600.0, 1500.0),
        "bone": (500.0, 2000.0),
    }

    def clean(value):
        if value is None:
            return None
        if isinstance(value, bytes):
            return "<bytes>"
        if isinstance(value, (list, tuple, MultiValue)):
            return "\\".join(str(v) for v in value)
        text = str(value).strip()
        return text if text else None

    def dicom_candidates(path):
        path = Path(path).expanduser()
        if path.is_file() and path.suffix.lower() == ".zip":
            with ZipFile(path) as zf:
                for info in zf.infolist():
                    if info.is_dir():
                        continue
                    name = info.filename
                    suffix = Path(name).suffix.lower()
                    basename = Path(name).name.upper()
                    if suffix in SKIP_EXTENSIONS and basename != "DICOMDIR":
                        continue
                    yield "zip", zf, name
        elif path.is_dir():
            for child in path.rglob("*"):
                if not child.is_file():
                    continue
                suffix = child.suffix.lower()
                basename = child.name.upper()
                if suffix in SKIP_EXTENSIONS and basename != "DICOMDIR":
                    continue
                yield "file", None, child
        else:
            yield "file", None, path

    def read_meta(kind, container, item):
        try:
            if kind == "zip":
                with container.open(item) as fp:
                    return pydicom.dcmread(
                        fp,
                        stop_before_pixels=True,
                        force=False,
                        specific_tags=["SeriesInstanceUID", "SeriesNumber", "InstanceNumber", "Rows", "Columns"],
                    )
            return pydicom.dcmread(
                str(item),
                stop_before_pixels=True,
                force=False,
                specific_tags=["SeriesInstanceUID", "SeriesNumber", "InstanceNumber", "Rows", "Columns"],
            )
        except Exception:
            return None

    def read_full(kind, container, item):
        if kind == "zip":
            with container.open(item) as fp:
                return pydicom.dcmread(fp, force=False)
        return pydicom.dcmread(str(item), force=False)

    def parse_instance(ds):
        value = clean(getattr(ds, "InstanceNumber", None))
        if value is None:
            return None
        try:
            return int(float(value))
        except ValueError:
            return None

    def series_number(ds):
        return clean(getattr(ds, "SeriesNumber", None)) or "<missing>"

    def series_uid(ds):
        return clean(getattr(ds, "SeriesInstanceUID", None))

    def selected_series(groups, requested):
        if requested:
            wanted = set(requested)
            return {key: value for key, value in groups.items() if value["series_number"] in wanted or key in wanted}
        ranked = sorted(groups.items(), key=lambda item: len(item[1]["items"]), reverse=True)
        return dict(ranked[:6])

    def window_pixels(ds, window):
        arr = ds.pixel_array.astype(np.float32)
        slope = float(getattr(ds, "RescaleSlope", 1) or 1)
        intercept = float(getattr(ds, "RescaleIntercept", 0) or 0)
        arr = arr * slope + intercept

        if arr.ndim == 3 and arr.shape[-1] in (3, 4):
            arr = arr[..., :3].mean(axis=-1)
        elif arr.ndim > 2:
            arr = arr[0]

        if window == "auto":
            low, high = np.percentile(arr, [1, 99])
        else:
            center, width = WINDOWS[window]
            low = center - width / 2
            high = center + width / 2

        if high <= low:
            low, high = float(np.min(arr)), float(np.max(arr))
        if high <= low:
            return np.zeros(arr.shape, dtype=np.uint8)

        arr = np.clip((arr - low) / (high - low), 0, 1)
        if clean(getattr(ds, "PhotometricInterpretation", None)) == "MONOCHROME1":
            arr = 1 - arr
        return (arr * 255).astype(np.uint8)

    def thumb_from_pixels(pixels, max_side):
        image = Image.fromarray(pixels).convert("L")
        image.thumbnail((max_side, max_side), Image.Resampling.LANCZOS)
        return image

    def sample_items(items, max_slices):
        items = sorted(items, key=lambda row: (row[0] if row[0] is not None else 10**9, row[1]))
        count = min(max_slices, len(items))
        if count == 0:
            return []
        indexes = np.linspace(0, len(items) - 1, count).round().astype(int).tolist()
        return [items[index] for index in indexes]

    def make_sheet(series_label, file_label, items, kind, container, outdir, max_slices, max_side, window):
        samples = sample_items(items, max_slices)
        thumbs = []
        labels = []
        failures = 0
        for instance, item in samples:
            try:
                ds = read_full(kind, container, item)
                pixels = window_pixels(ds, window)
                thumbs.append(thumb_from_pixels(pixels, max_side))
                labels.append(str(instance) if instance is not None else "?")
            except Exception:
                failures += 1

        if not thumbs:
            return None, failures

        cell_w = max_side + 40
        cell_h = max_side + 60
        cols = min(5, len(thumbs))
        rows = math.ceil(len(thumbs) / cols)
        sheet = Image.new("RGB", (cols * cell_w, rows * cell_h + 34), "white")
        draw = ImageDraw.Draw(sheet)
        draw.text((8, 8), f"Series {series_label} | sampled {len(thumbs)} of {len(items)} images | window {window}", fill="black")

        for index, (thumb, label) in enumerate(zip(thumbs, labels)):
            x = (index % cols) * cell_w
            y = 34 + (index // cols) * cell_h
            sheet.paste(thumb.convert("RGB"), (x + (cell_w - thumb.width) // 2, y + 22))
            draw.text((x + 8, y + 4), f"Instance {label}", fill="black")

        out = Path(outdir) / f"series_{file_label}_contact_{window}.jpg"
        sheet.save(out, quality=90)
        return out, failures

    def build_groups(input_path):
        groups = {}
        handles = []
        try:
            path = Path(input_path).expanduser()
            if path.is_file() and path.suffix.lower() == ".zip":
                zf = ZipFile(path)
                handles.append(zf)
                for info in zf.infolist():
                    if info.is_dir():
                        continue
                    item = info.filename
                    suffix = Path(item).suffix.lower()
                    basename = Path(item).name.upper()
                    if suffix in SKIP_EXTENSIONS and basename != "DICOMDIR":
                        continue
                    ds = read_meta("zip", zf, item)
                    if ds is None or str(item).upper().endswith("DICOMDIR"):
                        continue
                    sn = series_number(ds)
                    uid = series_uid(ds)
                    group_key = uid or f"fallback:{sn}:{Path(item).parent}"
                    group = groups.setdefault(group_key, {"series_number": sn, "kind": "zip", "container": zf, "items": []})
                    group["items"].append((parse_instance(ds), item))
            else:
                for kind, container, item in dicom_candidates(path):
                    ds = read_meta(kind, container, item)
                    if ds is None or str(item).upper().endswith("DICOMDIR"):
                        continue
                    sn = series_number(ds)
                    uid = series_uid(ds)
                    group_key = uid or f"fallback:{sn}:{Path(str(item)).parent}"
                    group = groups.setdefault(group_key, {"series_number": sn, "kind": kind, "container": container, "items": []})
                    group["items"].append((parse_instance(ds), item))
            return groups, handles
        except BadZipFile as exc:
            raise SystemExit(f"Not a valid zip file: {exc}")

    def main():
        parser = argparse.ArgumentParser(description="Generate derived local DICOM contact sheets for review.")
        parser.add_argument("input", help="DICOM zip, DICOMDIR export folder, or single DICOM file")
        parser.add_argument("--series", action="append", default=[], help="SeriesNumber to render; repeatable. Defaults to the six largest series.")
        parser.add_argument("--output-dir", help="Output directory. Defaults to a temporary directory under /tmp.")
        parser.add_argument("--max-slices", type=int, default=25, help="Maximum sampled slices per series")
        parser.add_argument("--max-side", type=int, default=180, help="Maximum thumbnail side length in pixels")
        parser.add_argument("--window", choices=sorted(WINDOWS.keys()) + ["auto"], default="soft", help="Display window")
        args = parser.parse_args()

        outdir = Path(args.output_dir) if args.output_dir else Path(tempfile.mkdtemp(prefix="dicom-contact-"))
        outdir.mkdir(parents=True, exist_ok=True)

        groups, handles = build_groups(args.input)
        try:
            chosen = selected_series(groups, args.series)
            if not chosen:
                raise SystemExit("No matching DICOM series found")

            print(f"Output directory: {outdir}")
            print("Contact sheets are derived local review images. Do not share them unless the source exam is anonymized and previews are checked for labels.")
            duplicate_counts = {}
            for group in chosen.values():
                duplicate_counts[group["series_number"]] = duplicate_counts.get(group["series_number"], 0) + 1

            ordered = sorted(
                chosen.items(),
                key=lambda item: (
                    int(item[1]["series_number"]) if item[1]["series_number"].isdigit() else 999999,
                    item[0],
                ),
            )
            for index, (_group_key, group) in enumerate(ordered, 1):
                series_no = group["series_number"]
                label = f"{series_no} group {index}" if duplicate_counts[series_no] > 1 else series_no
                file_label = f"{series_no}_{index}" if duplicate_counts[series_no] > 1 else series_no
                sheet, failures = make_sheet(label, file_label, group["items"], group["kind"], group["container"], outdir, args.max_slices, args.max_side, args.window)
                if sheet is None:
                    print(f"Series {label}: failed to render sampled images")
                else:
                    print(f"Series {label}: {len(group['items'])} images -> {sheet} ({failures} sampled failures)")
        finally:
            for handle in handles:
                handle.close()

    if __name__ == "__main__":
        main()
  '';

  inventoryScript = pkgs.writeShellScript "dicom-inventory" ''
    exec ${python}/bin/python3 ${inventoryPy} "$@"
  '';

  privacyScript = pkgs.writeShellScript "dicom-privacy-check" ''
    exec ${python}/bin/python3 ${privacyPy} "$@"
  '';

  contactSheetsScript = pkgs.writeShellScript "dicom-contact-sheets" ''
    exec ${python}/bin/python3 ${contactSheetsPy} "$@"
  '';

  skill = ''
    ---
    name: dicom-review
    description: "Deterministic DICOM workflow for CT/MRI exam exports. Use when inspecting DICOM zips/folders, checking anonymization, creating study/series inventories, or preparing non-medical scan questions for a doctor."
    ---

    # DICOM Review

    Use this skill for CT/MRI DICOM exports, including zipped DVD exports with `DICOMDIR`, extensionless DICOM files, or viewer-generated JPEG previews.

    The goal is a reproducible technical/image-review workflow that helps the user prepare better questions for a clinician. Do not provide diagnosis, treatment advice, urgency assessment, benign/malignant claims, or claims that a clinician missed something. Phrase outputs as neutral observations and questions to ask the doctor.

    ## Workflow

    ### Step 1: Confirm scope and consent

    Purpose: establish what the user wants inspected and get approval before accessing local scan files.

    Process:
    - Confirm the path to the DICOM zip/folder.
    - Ask for the radiology report if available; report correlation is safer and more useful than image-only review.
    - Remind the user that anonymized DICOM may still contain identifiers in metadata, filenames, reports, previews, or burned-in annotations.
    - Get explicit approval before reading/extracting/running commands against scan files.

    Exit criteria: user has approved a specific file/folder and scope.

    ### Step 2: Run privacy check first

    Purpose: identify obvious metadata/privacy issues before summarizing or sharing scan content.

    Process:
    - From this skill directory, run:

      ```bash
      ./dicom-privacy-check <dicom-zip-or-folder>
      ```

    - Do not use `--show-values` unless the user explicitly asks; it may print identifiers.
    - Do not use `--show-paths` unless the user explicitly accepts local path/member-name disclosure.
    - Summarize counts and risks, not private values.
    - If identifiers are present, ask whether to continue locally or stop and anonymize.

    Exit criteria: visible privacy summary with metadata fields present and preview/burned-in annotation risks.

    ### Step 3: Build deterministic study inventory

    Purpose: produce a stable technical inventory of the exam without reading pixel data.

    Process:
    - Run:

      ```bash
      ./dicom-inventory <dicom-zip-or-folder>
      ```

    - Use the generated inventory as the source of truth for modalities, counts, slice thickness, pixel spacing, contrast hints, and image completeness questions.
    - Do not use `--show-values` unless the user explicitly accepts descriptor/UID disclosure.
    - Flag technical issues as questions: low slice counts, scout/localizer-only series, missing expected series, parse errors, odd spacing, or `BurnedInAnnotation` metadata.

    Exit criteria: visible series inventory summary.

    ### Step 4: Generate local contact sheets when image-level review is requested

    Purpose: create local-only derived previews for neutral visual flagging without dumping raw DICOM pixels into the repo.

    Process:
    - Run selected series only:

      ```bash
      ./dicom-contact-sheets <dicom-zip-or-folder> --series 10 --series 4 --window soft
      ```

    - Use `--window lung` or `--window bone` only when that technical window is relevant to the question.
    - Treat generated JPEGs as sensitive derived images. Do not share them unless the source exam is anonymized and previews are checked for labels.
    - Describe visible items neutrally as “areas to ask about,” not diagnoses.

    Exit criteria: contact sheets exist in a temporary/output directory and any observations are phrased as doctor questions.

    ### Step 5: Correlate with the report if available

    Purpose: connect report language to series-level evidence without turning it into a medical read.

    Process:
    - Parse the report separately if the user provides PDF/HTML/TXT.
    - Extract findings/impression terms and map them to likely series by modality/body region/sequence names.
    - Use cautious wording: “the report mentions X; likely relevant series are Y/Z; ask your doctor to show you where X appears.”

    Exit criteria: report terms mapped to likely series or a note that no report was provided.

    ### Step 6: Produce doctor-question output

    Purpose: create a concise artifact the user can take to a doctor.

    Process:
    - Separate mechanical facts from questions.
    - Use this structure:

      ```text
      Technical inventory facts:
      - ...

      Privacy / completeness flags:
      - ...

      Questions for doctor:
      1. Series <n/name>: <neutral observation or report correlation>.
         Ask: "..."
      ```

    - Avoid terms that assert pathology. Prefer “measures approximately X”, “appears asymmetric”, “different density/signal”, “not clear from metadata”, and “ask whether this is expected.”

    Exit criteria: a neutral, non-diagnostic question list with series references.

    ## Tool reference

    - `./dicom-privacy-check <input>`: checks selected metadata fields for possible identifiers without printing values by default.
    - `./dicom-inventory <input>`: groups DICOM files by series and reports technical metadata without pixel data.
    - `./dicom-contact-sheets <input> --series <n>`: generates local derived JPEG contact sheets for selected series.
    - All three tools accept a zip, extracted DICOM folder, or single DICOM file.
    - `dicom-privacy-check` and `dicom-inventory` hide input/member paths by default; use `--show-paths` only when local path disclosure is acceptable.
    - `dicom-inventory` hides study/series UIDs and descriptor fields by default; use `--show-values` only when descriptor/UID disclosure is acceptable.
    - `dicom-privacy-check` and `dicom-inventory` support `--json` for machine-readable output.
  '';
in
{
  inherit
    inventoryScript
    privacyScript
    contactSheetsScript
    skill
    ;
}
