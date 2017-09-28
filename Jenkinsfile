node {
  stage 'Clone'
  checkout scm

  stage 'Build and Test'
  def mvnHome = tool 'M3'
  sh "${mvnHome}/bin/mvn -B clean install"
}
