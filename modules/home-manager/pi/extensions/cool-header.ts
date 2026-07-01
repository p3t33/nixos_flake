import type { ExtensionAPI, Theme } from "@earendil-works/pi-coding-agent";
import { VERSION } from "@earendil-works/pi-coding-agent";

// pi pixel mascot (8x8 sprite), packed two rows per line with half-block cells.
const PI_SPRITE = [
	"######..",
	"######..",
	"##..##..",
	"##..##..",
	"####..##",
	"####..##",
	"##....##",
	"##....##",
];

function getPiLogo(theme: Theme): string[] {
	const pixel = (top: boolean, bottom: boolean): string => {
		if (top && bottom) return "\u2588";
		if (top) return "\u2580";
		if (bottom) return "\u2584";
		return " ";
	};
	const rows: string[] = [];
	for (let i = 0; i < PI_SPRITE.length; i += 2) {
		const top = PI_SPRITE[i];
		const bottom = PI_SPRITE[i + 1] ?? "";
		const line = Array.from(top)
			.map((_, j) => pixel(top[j] === "#", bottom[j] === "#"))
			.join("");
		rows.push("  " + theme.fg("accent", line));
	}
	return rows;
}

export default function (pi: ExtensionAPI) {
	pi.on("session_start", async (_event, ctx) => {
		if (ctx.mode !== "tui") return;
		ctx.ui.setHeader((_tui, theme) => ({
			render(_width: number): string[] {
				const logo = getPiLogo(theme);
				const version = theme.fg("dim", `v${VERSION}`);
				return logo.map((row, i) => (i === 0 ? row + "   " + version : row));
			},
			invalidate() {},
		}));
	});
}
