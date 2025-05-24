/**
 * Theme Switcher for Cuisine Code
 * Copyright (c) 2025 Aidan Pace
 */

document.addEventListener('DOMContentLoaded', () => {
  const themeToggle = document.getElementById('theme-toggle');
  const themeStylesheet = document.getElementById('theme-stylesheet');
  
  // Check for user preference
  const prefersDarkMode = window.matchMedia('(prefers-color-scheme: dark)').matches;
  const savedTheme = localStorage.getItem('theme');
  
  // Set initial theme
  if (savedTheme) {
    setTheme(savedTheme);
  } else if (prefersDarkMode) {
    setTheme('dark');
  } else {
    setTheme('light');
  }
  
  // Theme toggle handler
  themeToggle.addEventListener('click', () => {
    const currentTheme = themeStylesheet.getAttribute('href').includes('dark') ? 'dark' : 'light';
    const newTheme = currentTheme === 'dark' ? 'light' : 'dark';
    
    setTheme(newTheme);
  });
  
  function setTheme(theme) {
    const themePath = `css/themes/${theme}.css`;
    themeStylesheet.setAttribute('href', themePath);
    localStorage.setItem('theme', theme);
    
    // Update body class for additional styling
    document.body.classList.remove('theme-light', 'theme-dark');
    document.body.classList.add(`theme-${theme}`);
  }
});
