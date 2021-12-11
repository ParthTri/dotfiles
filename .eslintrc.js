module.exports = {
    globals: {},
    env: {
	browser: true,
	es6: true,
    },
    extends: "eslint:recommended",
    parserOptions: {
	sourceType: "module",
	ecmaFeatures: {
	    jsx: true
	}
    },
    rules: {
	"linebreak-style" : ["error", "unix"],
	semi: ["error", "always"],
	"object-curely-spacing": ["error", "always"]
    }
};
