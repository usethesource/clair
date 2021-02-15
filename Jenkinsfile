node {
  stage ('Clone') {
    checkout scm
  }

  withMaven(maven: 'M3', jdk: 'jdk-oracle-8', mavenOpts: '-Xmx4G', options: [artifactsPublisher(disabled: true), junitPublisher(disabled: false)] ) {
        stage('Compile & Bootstrap') {
          sh "mvn clean compile package"
        }

        stage('Deploy') {
          if (env.BRANCH_NAME == "master" || env.BRANCH_NAME == "jenkins-deploy") {
            sh "mvn -DskipTests deploy"
          }
        }
  }

  build job: '../rascal-eclipse-libraries/master', wait: false
}
